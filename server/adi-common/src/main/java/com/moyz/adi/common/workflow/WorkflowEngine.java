package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.workflow.WfRuntimeMetricsSummary;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.WorkflowRuntimeNodeService;
import com.moyz.adi.common.service.WorkflowRuntimeService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.workflow.NodeExecutionMetrics;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.def.WfNodeIO;
import com.moyz.adi.common.workflow.metrics.LLMMetrics;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.bsc.async.AsyncGenerator;
import org.bsc.langgraph4j.*;
import org.bsc.langgraph4j.checkpoint.Checkpoint;
import org.bsc.langgraph4j.checkpoint.MemorySaver;
import org.bsc.langgraph4j.langchain4j.generators.StreamingChatGenerator;
import org.bsc.langgraph4j.serializer.std.ObjectStreamStateSerializer;
import org.bsc.langgraph4j.state.AgentState;
import org.bsc.langgraph4j.state.StateSnapshot;
import org.bsc.langgraph4j.streaming.StreamingOutput;
import com.moyz.adi.common.helper.SseManager;
import org.springframework.http.ResponseEntity;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.*;
import static com.moyz.adi.common.enums.ErrorEnum.*;
import static com.moyz.adi.common.workflow.WfComponentNameEnum.HUMAN_FEEDBACK;
import static org.bsc.langgraph4j.StateGraph.END;
import static org.bsc.langgraph4j.StateGraph.START;
import static org.bsc.langgraph4j.action.AsyncEdgeAction.edge_async;
import static org.bsc.langgraph4j.action.AsyncNodeAction.node_async;

@Slf4j
public class WorkflowEngine {
    private CompiledGraph<WfNodeState> app;
    private final Workflow workflow;
    private final List<WorkflowComponent> components;
    private final List<WorkflowNode> wfNodes;
    private final List<WorkflowEdge> wfEdges;
    private final SseManager sseManager;
    private final WorkflowRuntimeService workflowRuntimeService;
    private final WorkflowRuntimeNodeService workflowRuntimeNodeService;
    /**
     * In-execution saver: MemorySaver for a fresh run, PreloadedSaver for a resumed one.
     * Kept as a field so the interruption branch can read the latest checkpoint for the runtime row.
     * 执行期 saver（首轮 MemorySaver / 续跑 PreloadedSaver），中断分支从中取最新快照落库。
     */
    private MemorySaver saver;

    private final ObjectStreamStateSerializer<WfNodeState> stateSerializer = new ObjectStreamStateSerializer<>(WfNodeState::new);
    private final Map<String, List<StateGraph<WfNodeState>>> stateGraphNodes = new HashMap<>();
    private final Map<String, List<StateGraph<WfNodeState>>> stateGraphEdges = new HashMap<>();
    private final Map<String, String> rootToSubGraph = new HashMap<>();
    private final Map<String, GraphCompileNode> nodeToParallelBranch = new HashMap<>();

    private String sseUuid;
    private User user;
    private WfState wfState;
    private WfRuntimeResp wfRuntimeResp;
    /**
     * 运行实例的创建时间（epoch 毫秒），duration 计算的起点。
     * The runtime row's create time (epoch millis); the anchor for computing the run's duration.
     */
    private long runtimeCreateTime;

    public WorkflowEngine(
            Workflow workflow,
            SseManager sseManager,
            List<WorkflowComponent> components,
            List<WorkflowNode> nodes,
            List<WorkflowEdge> wfEdges,
            WorkflowRuntimeService workflowRuntimeService,
            WorkflowRuntimeNodeService workflowRuntimeNodeService) {
        this.workflow = workflow;
        this.sseManager = sseManager;
        this.components = components;
        this.wfNodes = nodes;
        this.wfEdges = wfEdges;
        this.workflowRuntimeService = workflowRuntimeService;
        this.workflowRuntimeNodeService = workflowRuntimeNodeService;
    }

    public void run(User user, List<ObjectNode> userInputs, String sseUuid) {
        this.user = user;
        this.sseUuid = sseUuid;
        log.info("WorkflowEngine run,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflow.getUuid(), userInputs);
        if (!this.workflow.getIsEnable()) {
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage(ErrorEnum.A_WF_DISABLED.getInfo()));
            throw new BaseException(ErrorEnum.A_WF_DISABLED);
        }

        Long workflowId = this.workflow.getId();
        this.wfRuntimeResp = workflowRuntimeService.create(user, workflowId);
        this.runtimeCreateTime = toEpochMillis(wfRuntimeResp.getCreateTime());
        this.sseManager.startSse(user, sseUuid, JsonUtil.toJson(wfRuntimeResp));

        String runtimeUuid = this.wfRuntimeResp.getUuid();
        try {
            WorkflowNode startNode = findStartAndEndNode().getLeft();
            List<NodeIOData> wfInputs = getAndCheckUserInput(userInputs, startNode);
            this.wfState = new WfState(user, wfInputs, runtimeUuid);
            workflowRuntimeService.updateInput(this.wfRuntimeResp.getId(), wfState);

            this.saver = new MemorySaver();
            compileApp(startNode, this.saver);
            //Pin the thread id to the runtime uuid so concurrent runs never collide on one checkpoint key
            RunnableConfig invokeConfig = RunnableConfig.builder()
                    .threadId(runtimeUuid)
                    .build();
            exe(invokeConfig, false);
        } catch (Throwable e) {
            errorWhenExe(e instanceof Exception ? (Exception) e : new RuntimeException(e));
        }
    }

    /**
     * LocalDateTime 转 epoch 毫秒，null 兜底当前时间 | Epoch millis; a null falls back to now.
     */
    static long toEpochMillis(LocalDateTime time) {
        return null == time ? System.currentTimeMillis() : time.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
    }

    private void exe(RunnableConfig invokeConfig, boolean resume) {
        //stream(null) resumes from the checkpoint; an empty map starts a fresh run
        AsyncGenerator<NodeOutput<WfNodeState>> outputs = app.stream(resume ? null : Map.of(), invokeConfig);
        streamingResult(wfState, outputs, sseUuid);

        StateSnapshot<WfNodeState> stateSnapshot = app.getState(invokeConfig);
        String nextNode = stateSnapshot.config().nextNode().orElse("");
        //A non-END next node means an interruption waiting for user feedback
        if (StringUtils.isNotBlank(nextNode) && !nextNode.equalsIgnoreCase(END)) {
            String intTip = WorkflowUtil.getHumanFeedbackTip(nextNode, wfNodes);
            //Ordering: persist -> signal -> close
            wfState.setProcessStatus(WORKFLOW_PROCESS_STATUS_WAITING_INPUT);
            byte[] checkpointBlob = snapshotCheckpoint(invokeConfig);
            WorkflowRuntime interruptedRuntime = workflowRuntimeService.updateOutputWithCheckpoint(wfRuntimeResp.getId(), wfState, runtimeCreateTime, computeTokenSummary(), checkpointBlob);
            //A dead connection must not escalate to FAIL; WAITING_INPUT is already persisted
            try {
                SseManager.parseAndSendPartialMsg(sseUuid, "[NODE_WAIT_FEEDBACK_BY_" + nextNode + "]", intTip);
                pushRuntimeMetrics(interruptedRuntime);
            } catch (Exception e) {
                log.warn("Send waiting-feedback events failed (client gone?), runtime:{}", wfState.getUuid(), e);
            }
            //Close without [DONE] (run-success in the client contract); this frees the SSE slot for the resume stream
            sseManager.closeStream(sseUuid);
        } else {
            WorkflowRuntime updatedRuntime = workflowRuntimeService.updateOutputWithCheckpoint(wfRuntimeResp.getId(), wfState, runtimeCreateTime, computeTokenSummary(), null);
            pushRuntimeMetrics(updatedRuntime);
            //Null only if the row was concurrently deleted; fall back to the in-memory output
            ObjectNode output = updatedRuntime != null ? updatedRuntime.getOutput() : null;
            sseManager.sendComplete(user.getId(), sseUuid, JsonUtil.toJson(output != null ? output : JsonUtil.createObjectNode()));
        }
    }

    /**
     * 从持久化 checkpoint 重建引擎并续跑剩余流程，输出走 resume 请求自带的新流
     * <p>
     * Rebuild the engine from the persisted checkpoint and continue the remaining flow; output goes to the resume request's own NEW SSE stream.
     * Any instance can serve a resume — the graph is rebuilt deterministically from the CURRENT workflow definition
     * (an edit during the wait window that breaks node identity fails the resume cleanly),
     * the business skeleton (completedNodes wiring + token metrics) is replayed from runtime_node rows,
     * and graph position / routing keys come from the checkpoint blob.
     * </p>
     *
     * @param user            当前用户 | current user
     * @param runtime         等待输入的运行行（含 id/uuid/createTime） | the waiting runtime row
     * @param checkpointBlob  序列化快照 | serialized checkpoint blob
     * @param runtimeNodeDtos 已完成节点行（回放数据源，按执行顺序） | completed node rows (execution order)
     * @param userInput       用户反馈 | user feedback
     * @param sseUuid         resume 请求的流标识 | the resume request's stream id
     */
    public void resume(User user, WorkflowRuntime runtime, byte[] checkpointBlob, List<WfRuntimeNodeDto> runtimeNodeDtos, String userInput, String sseUuid) {
        this.user = user;
        this.sseUuid = sseUuid;
        this.runtimeCreateTime = toEpochMillis(runtime.getCreateTime());
        WfRuntimeResp resp = new WfRuntimeResp();
        resp.setId(runtime.getId());
        resp.setUuid(runtime.getUuid());
        this.wfRuntimeResp = resp;
        String runtimeUuid = runtime.getUuid();
        try {
            this.wfState = new WfState(user, new ArrayList<>(), runtimeUuid);
            replayCompletedNodes(runtimeNodeDtos);

            WorkflowNode startNode = findStartAndEndNode().getLeft();
            Checkpoint checkpoint = WorkflowCheckpointCodec.fromBlob(checkpointBlob);
            RunnableConfig invokeConfig = RunnableConfig.builder()
                    .threadId(runtimeUuid)
                    .build();
            PreloadedSaver preloadedSaver = new PreloadedSaver(invokeConfig, checkpoint);
            this.saver = preloadedSaver;
            compileApp(startNode, preloadedSaver);
            //No [START] on the resume stream; the client already holds the runtime state and would duplicate the run entry
            app.updateState(invokeConfig, Map.of(HUMAN_FEEDBACK_KEY, userInput), null);
            exe(invokeConfig, true);
        } catch (Throwable e) {
            errorWhenExe(e instanceof Exception ? (Exception) e : new RuntimeException(e));
        }
    }

    /**
     * 从 runtime_node 行回放已完成节点：inputs/outputs（节点接线数据源）与 metrics（token 聚合）。
     * 图位置来自 checkpoint，不在此处。找不到定义行（等待期间工作流被编辑）则跳过该节点并告警
     * ——下游引用取不到值时各自降级为 warn，属已接受边界。
     * <p>
     * Replay completed nodes from runtime_node rows: inputs/outputs (node wiring source) and metrics (token aggregation).
     * Graph position comes from the checkpoint. A row whose definition is gone (workflow edited during the wait) is skipped with a warning.
     * </p>
     */
    private void replayCompletedNodes(List<WfRuntimeNodeDto> runtimeNodeDtos) {
        for (WfRuntimeNodeDto dto : runtimeNodeDtos) {
            WorkflowNode wfNode = wfNodes.stream().filter(n -> n.getId().equals(dto.getNodeId())).findFirst().orElse(null);
            if (null == wfNode) {
                log.warn("Replay: workflow node row not found, runtimeNodeUuid:{},nodeId:{}", dto.getUuid(), dto.getNodeId());
                continue;
            }
            WorkflowComponent wfComponent = components.stream().filter(c -> c.getId().equals(wfNode.getWorkflowComponentId())).findFirst().orElse(null);
            if (null == wfComponent) {
                log.warn("Replay: component not found, node uuid:{}", wfNode.getUuid());
                continue;
            }
            WfNodeState nodeState = new WfNodeState();
            nodeState.setUuid(dto.getUuid());
            nodeState.setProcessStatus(NODE_PROCESS_STATUS_SUCCESS);
            nodeState.setInputs(replayIO(dto.getInput()));
            nodeState.setOutputs(replayIO(dto.getOutput()));
            nodeState.setMetrics(null != dto.getMetadata() ? dto.getMetadata() : new NodeExecutionMetrics());
            wfState.getCompletedNodes().add(WfNodeFactory.create(wfComponent, wfNode, wfState, nodeState));
            wfState.getRuntimeNodes().add(dto);
        }
    }

    /**
     * 持久化的 {参数名: content} JSON 还原为 NodeIOData 列表，复用 WfNodeIODataUtil 的分型分发。
     * Restore persisted {name: content} JSON into a NodeIOData list, reusing the typed dispatch.
     */
    private List<NodeIOData> replayIO(ObjectNode persisted) {
        List<NodeIOData> result = new ArrayList<>();
        if (null == persisted) {
            return result;
        }
        persisted.fields().forEachRemaining(entry -> {
            ObjectNode shaped = JsonUtil.createObjectNode();
            shaped.put("name", entry.getKey());
            shaped.set("content", entry.getValue());
            try {
                result.add(WfNodeIODataUtil.createNodeIOData(shaped));
            } catch (Exception e) {
                log.warn("Replay: skip unparsable io entry, name:{}", entry.getKey());
            }
        });
        return result;
    }

    /**
     * 从执行期 saver 取最新 checkpoint 序列化为 runtime 行 blob。
     * Serialize the latest checkpoint from the in-execution saver for the runtime row.
     */
    private byte[] snapshotCheckpoint(RunnableConfig invokeConfig) {
        Checkpoint checkpoint = saver.get(invokeConfig)
                .orElseThrow(() -> new IllegalStateException("Checkpoint not found at interruption, runtime:" + wfState.getUuid()));
        return WorkflowCheckpointCodec.toBlob(checkpoint);
    }

    /**
     * Blocking execution: build the workflow graph, run it synchronously,
     * and return the final output as a JSON response.
     */
    public Map<String, Object> blockingRun(User user, List<ObjectNode> userInputs) {
        this.user = user;
        //Dummy emitter marked completed so SSE sends in runNode silently skip
        this.sseUuid = "blocking-" + com.moyz.adi.common.util.UuidUtil.createShort();
        SseEmitter dummyEmitter = new SseEmitter(0L);
        SpringUtil.getBean(com.moyz.adi.common.helper.SseManager.class).register(this.sseUuid, dummyEmitter);
        sseManager.sendComplete(user.getId(), this.sseUuid);
        log.info("WorkflowEngine blockingRun,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflow.getUuid(), userInputs);
        if (!this.workflow.getIsEnable()) {
            throw new BaseException(ErrorEnum.A_WF_DISABLED);
        }

        Long workflowId = this.workflow.getId();
        this.wfRuntimeResp = workflowRuntimeService.create(user, workflowId);
        this.runtimeCreateTime = toEpochMillis(wfRuntimeResp.getCreateTime());

        String runtimeUuid = this.wfRuntimeResp.getUuid();
        try {
            WorkflowNode startNode = findStartAndEndNode().getLeft();
            List<NodeIOData> wfInputs = getAndCheckUserInput(userInputs, startNode);
            this.wfState = new WfState(user, wfInputs, runtimeUuid);
            workflowRuntimeService.updateInput(this.wfRuntimeResp.getId(), wfState);

            this.saver = new MemorySaver();
            compileApp(startNode, this.saver);
            //Reject human feedback AFTER compilation (interruptNodes is populated by buildStateGraph)
            if (!wfState.getInterruptNodes().isEmpty()) {
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            RunnableConfig invokeConfig = RunnableConfig.builder().threadId(runtimeUuid).build();
            app.invoke(Map.of(), invokeConfig);

            //Drain streaming generators so mapResult callbacks fire (token recording)
            for (StreamingChatGenerator<AgentState> generator : wfState.getNodeToStreamingGenerator().values()) {
                for (Object ignored : generator) {
                }
            }

            //Output comes from the last completed node
            List<AbstractWfNode> completedNodes = wfState.getCompletedNodes();
            if (!completedNodes.isEmpty()) {
                wfState.setOutput(completedNodes.get(completedNodes.size() - 1).getState().getOutputs());
            }

            WorkflowRuntime updatedRuntime = workflowRuntimeService.updateOutput(wfRuntimeResp.getId(), wfState, runtimeCreateTime, computeTokenSummary());

            Map<String, Object> data = new LinkedHashMap<>();
            data.put("task_id", runtimeUuid);
            data.put("status", "completed");
            if (null != updatedRuntime) {
                data.put("outputs", updatedRuntime.getOutput());
            }
            return data;
        } catch (Throwable e) {
            log.error("blockingRun execution exception, workflowUuid:{}", workflow.getUuid(), e);
            String errorMsg = e.getMessage();
            workflowRuntimeService.updateStatus(wfRuntimeResp.getId(), WORKFLOW_PROCESS_STATUS_FAIL, errorMsg, runtimeCreateTime, computeTokenSummary());

            Map<String, Object> errorResult = new LinkedHashMap<>();
            errorResult.put("message", errorMsg);
            return errorResult;
        }
    }

    /**
     * 构建+编译主状态图：从 startNode 展开编译树（检测节点重复访问防止无限循环），
     * 注册全部节点/边，再以给定 saver（首轮 MemorySaver / 续跑 PreloadedSaver）与 interruptBefore 编译为可执行图，赋给 {@link #app}。
     * run / resume / blockingRun 三入口共用。
     * <p>
     * Build and compile the main state graph shared by run / resume / blockingRun:
     * expand the compile tree from startNode (with revisit detection against infinite loops), register all nodes/edges,
     * then compile with the given saver (MemorySaver for a fresh run, PreloadedSaver for a resumed one) and interruptBefore into {@link #app}.
     * </p>
     *
     * @param startNode 起始节点 | the start node
     * @param saver     执行期检查点保存器 | in-execution checkpoint saver
     */
    private void compileApp(WorkflowNode startNode, MemorySaver saver) throws GraphStateException {
        CompileNode rootCompileNode = new CompileNode();
        rootCompileNode.setId(startNode.getUuid());

        Map<String, Integer> nodeVisitCount = new HashMap<>();
        buildCompileNode(rootCompileNode, startNode, nodeVisitCount);

        StateGraph<WfNodeState> mainStateGraph = new StateGraph<>(stateSerializer);
        this.wfState.addEdge(START, startNode.getUuid());
        buildStateGraph(null, mainStateGraph, rootCompileNode);

        CompileConfig compileConfig = CompileConfig.builder()
                .checkpointSaver(saver)
                .interruptBefore(wfState.getInterruptNodes().toArray(String[]::new))
                .build();
        app = mainStateGraph.compile(compileConfig);
    }

    private void errorWhenExe(Exception e) {
        log.error("error", e);
        String errorMsg = e.getMessage();
        //A null message (bare NPE) would break the contains() below; fall back to the exception class name
        if (null == errorMsg) {
            errorMsg = e.getClass().getSimpleName();
        }
        if (errorMsg.contains("parallel node doesn't support conditional branch")) {
            errorMsg = "Parallel nodes cannot contain conditional branches";
        }
        //Best-effort terminal persist (clears the checkpoint); the SSE error must still reach the client
        try {
            WorkflowRuntime failedRuntime = workflowRuntimeService.updateStatusWithCheckpoint(wfRuntimeResp.getId(), WORKFLOW_PROCESS_STATUS_FAIL, errorMsg, runtimeCreateTime, computeTokenSummary());
            pushRuntimeMetrics(failedRuntime);
        } catch (Throwable persistenceEx) {
            log.error("Failed to persist runtime failure status, sending SSE error anyway", persistenceEx);
        }
        sseManager.sendErrorAndComplete(user.getId(), sseUuid, errorMsg);
    }

    /**
     * Aggregate input/output token totals from completed nodes' in-memory metrics, avoiding a DB
     * round-trip. Only LLM-typed nodes (llm, agent) contribute; other node types are skipped.
     */
    private WfRuntimeMetricsSummary computeTokenSummary() {
        long inputTokens = 0;
        long outputTokens = 0;
        long duration = 0;
        for (AbstractWfNode node : wfState.getCompletedNodes()) {
            NodeExecutionMetrics metrics = node.getState().getMetrics();
            if (metrics != null) {
                duration += metrics.getDurationMs();
                if (metrics instanceof LLMMetrics llm) {
                    if (llm.getInputTokens() != null) {
                        inputTokens += llm.getInputTokens();
                    }
                    if (llm.getOutputTokens() != null) {
                        outputTokens += llm.getOutputTokens();
                    }
                }
            }
        }
        return new WfRuntimeMetricsSummary(inputTokens, outputTokens, duration);
    }

    /**
     * Push the runtime's terminal metrics snapshot via SSE. The DB columns are NOT NULL DEFAULT 0
     * so a non-null runtime always has values; null safety is a guard against the caller passing a
     * runtime that was returned null by updateOutput/updateStatus (row concurrently deleted).
     */
    private void pushRuntimeMetrics(WorkflowRuntime runtime) {
        if (runtime == null) {
            return;
        }
        SseManager.parseAndSendPartialMsg(sseUuid, AdiConstant.SSEEventName.RUNTIME_METRICS,
                JsonUtil.toJson(Map.of("inputTokens", runtime.getInputTokens(),
                        "outputTokens", runtime.getOutputTokens(),
                        "duration", runtime.getDuration())));
    }

    private Map<String, Object> runNode(WorkflowNode wfNode, WfNodeState nodeState) {
        Map<String, Object> resultMap = new HashMap<>();
        try {
            WorkflowComponent wfComponent = components.stream().filter(item -> item.getId().equals(wfNode.getWorkflowComponentId())).findFirst().orElseThrow();
            AbstractWfNode abstractWfNode = WfNodeFactory.create(wfComponent, wfNode, wfState, nodeState);
            WfRuntimeNodeDto runtimeNodeDto = workflowRuntimeNodeService.createByState(user, wfNode.getId(), wfRuntimeResp.getId(), nodeState);
            wfState.getRuntimeNodes().add(runtimeNodeDto);

            SseManager.parseAndSendPartialMsg(sseUuid, "[NODE_RUN_" + wfNode.getUuid() + "]", JsonUtil.toJson(runtimeNodeDto));

            NodeProcessResult processResult = abstractWfNode.process((is) -> {
                workflowRuntimeNodeService.updateInput(runtimeNodeDto.getId(), nodeState);
                for (NodeIOData input : nodeState.getInputs()) {
                    SseManager.parseAndSendPartialMsg(sseUuid, "[NODE_INPUT_" + wfNode.getUuid() + "]", JsonUtil.toJson(input));
                }
            }, (is) -> {
                workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), nodeState);

                //Nodes inside parallel branches must push outputs to the client explicitly
                String nodeUuid = wfNode.getUuid();
                List<NodeIOData> nodeOutputs = nodeState.getOutputs();
                for (NodeIOData output : nodeOutputs) {
                    log.info("callback node:{},output:{}", nodeUuid, output.getContent());
                    SseManager.parseAndSendPartialMsg(sseUuid, "[NODE_OUTPUT_" + nodeUuid + "]", JsonUtil.toJson(output));
                }
                //Push metrics here; streaming nodes are handled by streamingResult (tokens only available after completion)
                if (nodeState.getMetrics() != null && nodeState.getMetrics().getDurationMs() > 0
                        && !wfState.getNodeToStreamingGenerator().containsKey(nodeUuid)) {
                    SseManager.parseAndSendPartialMsg(sseUuid, AdiConstant.SSEEventName.NODE_METRICS_PREFIX + nodeUuid + "]", JsonUtil.toJson(nodeState.getMetrics()));
                }
            });
            if (StringUtils.isNotBlank(processResult.getNextNodeUuid())) {
                resultMap.put("next", processResult.getNextNodeUuid());
            }
        } catch (Exception e) {
            log.error("Node run error", e);
            throw new BaseException(ErrorEnum.B_WF_RUN_ERROR);
        }
        resultMap.put("name", wfNode.getTitle());
        //Store metadata only, never langgraph4j state data
        StreamingChatGenerator<AgentState> generator = wfState.getNodeToStreamingGenerator().get(wfNode.getUuid());
        if (null != generator) {
            resultMap.put("_streaming_messages", generator);
            return resultMap;
        }
        return resultMap;
    }

    /**
     * 流式输出结果
     *
     * @param outputs    输出
     * @param sseUuid SSE 请求标识 / SSE request identifier
     */
    private void streamingResult(WfState wfState, AsyncGenerator<NodeOutput<WfNodeState>> outputs, String sseUuid) {
        for (NodeOutput<WfNodeState> out : outputs) {
            if (out instanceof StreamingOutput<WfNodeState> streamingOutput) {
                String node = streamingOutput.node();
                String chunk = streamingOutput.chunk();
                log.info("node:{},chunk:{}", node, streamingOutput.chunk());
                SseManager.parseAndSendPartialMsg(sseUuid, "[NODE_CHUNK_" + node + "]", chunk);
            } else {
                AbstractWfNode abstractWfNode = wfState.getCompletedNodes().stream().filter(item -> item.getNode().getUuid().endsWith(out.node())).findFirst().orElse(null);
                if (null != abstractWfNode) {
                    WfRuntimeNodeDto runtimeNodeDto = wfState.getRuntimeNodeByNodeUuid(out.node());
                    if (null != runtimeNodeDto) {
                        workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), abstractWfNode.getState());
                        wfState.setOutput(abstractWfNode.getState().getOutputs());
                        //Push metrics when a streaming node completes (tokens only available now)
                        if (abstractWfNode.getState().getMetrics() != null && abstractWfNode.getState().getMetrics().getDurationMs() > 0) {
                            SseManager.parseAndSendPartialMsg(sseUuid, AdiConstant.SSEEventName.NODE_METRICS_PREFIX + out.node() + "]", JsonUtil.toJson(abstractWfNode.getState().getMetrics()));
                        }
                    } else {
                        log.warn("Can not find runtime node, node uuid:{}", out.node());
                    }
                } else {
                    log.warn("Can not find node state,node uuid:{}", out.node());
                }
            }
        }
    }

    /**
     * 校验用户输入并组装成工作流的输入
     *
     * @param userInputs 用户输入
     * @param startNode  开始节点定义
     * @return 正确的用户输入列表
     */
    private List<NodeIOData> getAndCheckUserInput(List<ObjectNode> userInputs, WorkflowNode startNode) {
        List<WfNodeIO> defList = startNode.getInputConfig().getUserInputs();
        List<NodeIOData> wfInputs = new ArrayList<>();
        for (WfNodeIO paramDefinition : defList) {
            String paramNameFromDef = paramDefinition.getName();
            boolean requiredParamMissing = paramDefinition.getRequired();
            for (ObjectNode userInput : userInputs) {
                NodeIOData nodeIOData = WfNodeIODataUtil.createNodeIOData(userInput);
                if (!paramNameFromDef.equalsIgnoreCase(nodeIOData.getName())) {
                    continue;
                }
                Integer dataType = nodeIOData.getContent().getType();
                if (null == dataType) {
                    throw new BaseException(A_WF_INPUT_INVALID);
                }
                requiredParamMissing = false;
                boolean valid = paramDefinition.checkValue(nodeIOData);
                if (!valid) {
                    log.error("Invalid user input, workflowId:{}", startNode.getWorkflowId());
                    throw new BaseException(ErrorEnum.A_WF_INPUT_INVALID);
                }
                wfInputs.add(nodeIOData);
            }
            if (requiredParamMissing) {
                log.error("Required parameter in flow definition not provided, name:{}", paramNameFromDef);
                throw new BaseException(A_WF_INPUT_MISSING);
            }
        }
        return wfInputs;
    }

    /**
     * 查找开始及结束节点 <br/>
     * 开始节点只能有一个，结束节点可能多个
     *
     * @return 开始节点及结束节点列表
     */
    public Pair<WorkflowNode, Set<WorkflowNode>> findStartAndEndNode() {
        WorkflowNode startNode = null;
        Set<WorkflowNode> endNodes = new HashSet<>();
        for (WorkflowNode node : wfNodes) {
            Optional<WorkflowComponent> wfComponent = components.stream().filter(item -> item.getId().equals(node.getWorkflowComponentId())).findFirst();
            if (wfComponent.isPresent() && WfComponentNameEnum.START.getName().equals(wfComponent.get().getName())) {
                if (null != startNode) {
                    throw new BaseException(ErrorEnum.A_WF_MULTIPLE_START_NODE);
                }
                startNode = node;
            } else if (wfComponent.isPresent() && WfComponentNameEnum.END.getName().equals(wfComponent.get().getName())) {
                endNodes.add(node);
            }
        }
        if (null == startNode) {
            log.error("No start node found, workflowId:{}", wfNodes.get(0).getWorkflowId());
            throw new BaseException(ErrorEnum.A_WF_START_NODE_NOT_FOUND);
        }
        //Find all end nodes
        wfNodes.forEach(item -> {
            String nodeUuid = item.getUuid();
            boolean source = false;
            boolean target = false;
            for (WorkflowEdge edgeDef : wfEdges) {
                if (edgeDef.getSourceNodeUuid().equals(nodeUuid)) {
                    source = true;
                } else if (edgeDef.getTargetNodeUuid().equals(nodeUuid)) {
                    target = true;
                }
            }
            if (!source && target) {
                endNodes.add(item);
            }
        });
        log.info("start node:{}", startNode);
        log.info("end nodes:{}", endNodes);
        if (endNodes.isEmpty()) {
            log.error("No end node found, workflowId:{}", startNode.getWorkflowId());
            throw new BaseException(A_WF_END_NODE_NOT_FOUND);
        }
        return Pair.of(startNode, endNodes);
    }

    private static final int MAX_NODE_VISITS = 10;

    private void buildCompileNode(
            CompileNode parentNode,
            WorkflowNode node,
            Map<String, Integer> nodeVisitCount) {
        int visits = nodeVisitCount.merge(node.getUuid(), 1, Integer::sum);
        if (visits > MAX_NODE_VISITS) {
            log.error("Node {} visited more than {} times, possible infinite loop in workflow graph", node.getUuid(), MAX_NODE_VISITS);
            throw new BaseException(ErrorEnum.B_WF_RUN_ERROR);
        }
        log.info("buildByNode, parentNode:{}, node:{},title:{}", parentNode.getId(), node.getUuid(), node.getTitle());
        CompileNode newNode;
        List<String> upstreamNodeUuids = getUpstreamNodeUuids(node.getUuid());
        if (upstreamNodeUuids.isEmpty()) {
            log.error("Node {} has no upstream node", node.getUuid());
            newNode = parentNode;
        } else if (upstreamNodeUuids.size() == 1) {
            String upstreamUuid = upstreamNodeUuids.get(0);
            boolean pointToParallel = pointToParallelBranch(upstreamUuid);
            if (pointToParallel) {
                String rootId = node.getUuid();
                GraphCompileNode graphCompileNode = getOrCreateGraphCompileNode(rootId);
                appendToNextNodes(parentNode, graphCompileNode);
                newNode = graphCompileNode;
            } else if (parentNode instanceof GraphCompileNode graphCompileNode) {
                newNode = CompileNode.builder().id(node.getUuid()).conditional(false).nextNodes(new ArrayList<>()).build();
                graphCompileNode.appendToLeaf(newNode);
            } else {
                newNode = CompileNode.builder().id(node.getUuid()).conditional(false).nextNodes(new ArrayList<>()).build();
                appendToNextNodes(parentNode, newNode);
            }
        } else {
            newNode = CompileNode.builder().id(node.getUuid()).conditional(false).nextNodes(new ArrayList<>()).build();
            GraphCompileNode parallelBranch = nodeToParallelBranch.get(parentNode.getId());
            appendToNextNodes(Objects.requireNonNullElse(parallelBranch, parentNode), newNode);
        }

        if (null == newNode) {
            log.error("Node {} does not exist", node.getUuid());
            return;
        }
        List<String> downstreamUuids = getDownstreamNodeUuids(node.getUuid());
        for (String downstream : downstreamUuids) {
            Optional<WorkflowNode> n = wfNodes.stream().filter(item -> item.getUuid().equals(downstream)).findFirst();
            n.ifPresent(workflowNode -> buildCompileNode(newNode, workflowNode, nodeVisitCount));
        }
    }

    /**
     * 构建完整的stategraph
     *
     * @param upstreamCompileNode 上游节点
     * @param stateGraph          当前状态图
     * @param compileNode         当前节点
     * @throws GraphStateException 状态图异常
     */
    private void buildStateGraph(CompileNode upstreamCompileNode, StateGraph<WfNodeState> stateGraph, CompileNode compileNode) throws GraphStateException {
        log.info("buildStateGraph,upstreamCompileNode:{},node:{}", upstreamCompileNode, compileNode.getId());
        String stateGraphNodeUuid = compileNode.getId();
        if (null == upstreamCompileNode) {
            addNodeToStateGraph(stateGraph, stateGraphNodeUuid);
            addEdgeToStateGraph(stateGraph, START, compileNode.getId());
        } else {
            if (compileNode instanceof GraphCompileNode graphCompileNode) {
                String stateGraphId = graphCompileNode.getId();
                CompileNode root = graphCompileNode.getRoot();
                String rootId = root.getId();
                String existSubGraphId = rootToSubGraph.get(rootId);

                if (StringUtils.isBlank(existSubGraphId)) {
                    StateGraph<WfNodeState> subgraph = new StateGraph<>(stateSerializer);
                    addNodeToStateGraph(subgraph, rootId);
                    addEdgeToStateGraph(subgraph, START, rootId);
                    for (CompileNode child : root.getNextNodes()) {
                        buildStateGraph(root, subgraph, child);
                    }
                    addEdgeToStateGraph(subgraph, graphCompileNode.getTail().getId(), END);
                    stateGraph.addNode(stateGraphId, subgraph.compile());
                    rootToSubGraph.put(rootId, stateGraphId);

                    stateGraphNodeUuid = stateGraphId;
                } else {
                    stateGraphNodeUuid = existSubGraphId;
                }
            } else {
                addNodeToStateGraph(stateGraph, stateGraphNodeUuid);
            }

            //Conditional edges are created separately
            if (Boolean.FALSE.equals(upstreamCompileNode.getConditional())) {
                addEdgeToStateGraph(stateGraph, upstreamCompileNode.getId(), stateGraphNodeUuid);
            }
        }
        List<CompileNode> nextNodes = compileNode.getNextNodes();
        if (nextNodes.size() > 1) {
            boolean conditional = nextNodes.stream().noneMatch(item -> item instanceof GraphCompileNode);
            compileNode.setConditional(conditional);
            for (CompileNode nextNode : nextNodes) {
                buildStateGraph(compileNode, stateGraph, nextNode);
            }
            //Conditional/classification nodes do not run in parallel; link them with conditional edges
            if (conditional) {
                List<String> targets = nextNodes.stream().map(CompileNode::getId).toList();
                Map<String, String> mappings = new HashMap<>();
                for (String target : targets) {
                    mappings.put(target, target);
                }
                stateGraph.addConditionalEdges(
                        stateGraphNodeUuid,
                        edge_async(state -> state.data().get("next").toString()),
                        mappings
                );
            }
        } else if (nextNodes.size() == 1) {
            for (CompileNode nextNode : nextNodes) {
                buildStateGraph(compileNode, stateGraph, nextNode);
            }
        } else {
            addEdgeToStateGraph(stateGraph, stateGraphNodeUuid, END);
        }
    }

    private GraphCompileNode getOrCreateGraphCompileNode(String rootId) {
        GraphCompileNode exist = nodeToParallelBranch.get(rootId);
        if (null == exist) {
            GraphCompileNode graphCompileNode = new GraphCompileNode();
            graphCompileNode.setId("parallel_" + rootId);
            graphCompileNode.setRoot(CompileNode.builder().id(rootId).conditional(false).nextNodes(new ArrayList<>()).build());
            nodeToParallelBranch.put(rootId, graphCompileNode);
            exist = graphCompileNode;
        }
        return exist;

    }

    private List<String> getUpstreamNodeUuids(String nodeUuid) {
        return this.wfEdges.stream()
                .filter(edge -> edge.getTargetNodeUuid().equals(nodeUuid))
                .map(WorkflowEdge::getSourceNodeUuid)
                .toList();
    }

    private List<String> getDownstreamNodeUuids(String nodeUuid) {
        return this.wfEdges.stream()
                .filter(edge -> edge.getSourceNodeUuid().equals(nodeUuid))
                .map(WorkflowEdge::getTargetNodeUuid)
                .toList();
    }

    //Determine if the node points to a parallel branch
    private boolean pointToParallelBranch(String nodeUuid) {
        int edgeCount = 0;
        for (WorkflowEdge edge : this.wfEdges) {
            if (edge.getSourceNodeUuid().equals(nodeUuid) && StringUtils.isBlank(edge.getSourceHandle())) {
                edgeCount = edgeCount + 1;
            }
        }
        return edgeCount > 1;
    }

    /**
     * 添加节点到状态图
     *
     * @param stateGraph
     * @param stateGraphNodeUuid
     * @throws GraphStateException
     */
    private void addNodeToStateGraph(StateGraph<WfNodeState> stateGraph, String stateGraphNodeUuid) throws GraphStateException {
        List<StateGraph<WfNodeState>> stateGraphList = stateGraphNodes.computeIfAbsent(stateGraphNodeUuid, k -> new ArrayList<>());
        boolean exist = stateGraphList.stream().anyMatch(item -> item == stateGraph);
        if (exist) {
            log.info("state graph node exist,stateGraphNodeUuid:{}", stateGraphNodeUuid);
            return;
        }
        log.info("addNodeToStateGraph,node uuid:{}", stateGraphNodeUuid);
        WorkflowNode wfNode = getNodeByUuid(stateGraphNodeUuid);
        stateGraph.addNode(stateGraphNodeUuid, node_async((state) -> runNode(wfNode, state)));
        stateGraphList.add(stateGraph);

        WorkflowComponent wfComponent = components.stream().filter(item -> item.getId().equals(wfNode.getWorkflowComponentId())).findFirst().orElseThrow();
        if (HUMAN_FEEDBACK.getName().equals(wfComponent.getName())) {
            this.wfState.addInterruptNode(stateGraphNodeUuid);
        }
    }

    private void addEdgeToStateGraph(StateGraph<WfNodeState> stateGraph, String source, String target) throws GraphStateException {
        String key = source + "_" + target;
        List<StateGraph<WfNodeState>> stateGraphList = stateGraphEdges.computeIfAbsent(key, k -> new ArrayList<>());
        boolean exist = stateGraphList.stream().anyMatch(item -> item == stateGraph);
        if (exist) {
            log.info("state graph edge exist,source:{},target:{}", source, target);
            return;
        }
        log.info("addEdgeToStateGraph,source:{},target:{}", source, target);
        stateGraph.addEdge(source, target);
        stateGraphList.add(stateGraph);
    }

    private WorkflowNode getNodeByUuid(String nodeUuid) {
        return wfNodes.stream()
                .filter(item -> item.getUuid().equals(nodeUuid))
                .findFirst()
                .orElseThrow(() -> new BaseException(ErrorEnum.A_WF_NODE_NOT_FOUND));
    }

    private void appendToNextNodes(CompileNode compileNode, CompileNode newNode) {
        boolean exist = compileNode.getNextNodes().stream().anyMatch(item -> item.getId().equals(newNode.getId()));
        if (!exist) {
            compileNode.getNextNodes().add(newNode);
        }

    }

    public CompiledGraph<WfNodeState> getApp() {
        return app;
    }
}
