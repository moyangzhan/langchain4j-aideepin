package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.WorkflowRuntimeNodeService;
import com.moyz.adi.common.service.WorkflowRuntimeService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.def.WfNodeIO;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.bsc.async.AsyncGenerator;
import org.bsc.langgraph4j.*;
import org.bsc.langgraph4j.checkpoint.MemorySaver;
import org.bsc.langgraph4j.langchain4j.generators.StreamingChatGenerator;
import org.bsc.langgraph4j.serializer.std.ObjectStreamStateSerializer;
import org.bsc.langgraph4j.state.AgentState;
import org.bsc.langgraph4j.state.StateSnapshot;
import org.bsc.langgraph4j.streaming.StreamingOutput;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import org.springframework.http.ResponseEntity;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

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
    private final SSEEmitterHelper sseEmitterHelper;
    private final WorkflowRuntimeService workflowRuntimeService;
    private final WorkflowRuntimeNodeService workflowRuntimeNodeService;

    private final ObjectStreamStateSerializer<WfNodeState> stateSerializer = new ObjectStreamStateSerializer<>(WfNodeState::new);
    private final Map<String, List<StateGraph<WfNodeState>>> stateGraphNodes = new HashMap<>();
    private final Map<String, List<StateGraph<WfNodeState>>> stateGraphEdges = new HashMap<>();
    private final Map<String, String> rootToSubGraph = new HashMap<>();
    private final Map<String, GraphCompileNode> nodeToParallelBranch = new HashMap<>();

    private SseEmitter sseEmitter;
    private User user;
    private WfState wfState;
    private WfRuntimeResp wfRuntimeResp;

    public WorkflowEngine(
            Workflow workflow,
            SSEEmitterHelper sseEmitterHelper,
            List<WorkflowComponent> components,
            List<WorkflowNode> nodes,
            List<WorkflowEdge> wfEdges,
            WorkflowRuntimeService workflowRuntimeService,
            WorkflowRuntimeNodeService workflowRuntimeNodeService) {
        this.workflow = workflow;
        this.sseEmitterHelper = sseEmitterHelper;
        this.components = components;
        this.wfNodes = nodes;
        this.wfEdges = wfEdges;
        this.workflowRuntimeService = workflowRuntimeService;
        this.workflowRuntimeNodeService = workflowRuntimeNodeService;
    }

    public void run(User user, List<ObjectNode> userInputs, SseEmitter sseEmitter) {
        this.user = user;
        this.sseEmitter = sseEmitter;
        log.info("WorkflowEngine run,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflow.getUuid(), userInputs);
        if (!this.workflow.getIsEnable()) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage(ErrorEnum.A_WF_DISABLED.getInfo()));
            throw new BaseException(ErrorEnum.A_WF_DISABLED);
        }

        Long workflowId = this.workflow.getId();
        this.wfRuntimeResp = workflowRuntimeService.create(user, workflowId);
        this.sseEmitterHelper.startSse(user, sseEmitter, JsonUtil.toJson(wfRuntimeResp));

        String runtimeUuid = this.wfRuntimeResp.getUuid();
        try {
            Pair<WorkflowNode, Set<WorkflowNode>> startAndEnds = findStartAndEndNode();
            WorkflowNode startNode = startAndEnds.getLeft();
            List<NodeIOData> wfInputs = getAndCheckUserInput(userInputs, startNode);
            //工作流运行实例状态
            this.wfState = new WfState(user, wfInputs, runtimeUuid);
            workflowRuntimeService.updateInput(this.wfRuntimeResp.getId(), wfState);
            CompileNode rootCompileNode = new CompileNode();
            rootCompileNode.setId(startNode.getUuid());

            //构建整棵树（检测节点重复访问防止无限循环）
            Map<String, Integer> nodeVisitCount = new HashMap<>();
            buildCompileNode(rootCompileNode, startNode, nodeVisitCount);

//Main state graph
            //主状态图
            StateGraph<WfNodeState> mainStateGraph = new StateGraph<>(stateSerializer);
            this.wfState.addEdge(START, startNode.getUuid());
            //构建包括所有节点的状态图
            buildStateGraph(null, mainStateGraph, rootCompileNode);

            MemorySaver saver = new MemorySaver();
            CompileConfig compileConfig = CompileConfig.builder()
                    .checkpointSaver(saver)
                    .interruptBefore(wfState.getInterruptNodes().toArray(String[]::new))
                    .build();
            app = mainStateGraph.compile(compileConfig);
            RunnableConfig invokeConfig = RunnableConfig.builder()
                    .build();
            exe(invokeConfig, false);
        } catch (Throwable e) {
            errorWhenExe(e instanceof Exception ? (Exception) e : new RuntimeException(e));
        }
    }

    private void exe(RunnableConfig invokeConfig, boolean resume) {
//Do not use langgraph4j state update methods, no need to pass input
        //不使用langgraph4j state的update相关方法，无需传入input
        AsyncGenerator<NodeOutput<WfNodeState>> outputs = app.stream(resume ? null : Map.of(), invokeConfig);
        streamingResult(wfState, outputs, sseEmitter);

        StateSnapshot<WfNodeState> stateSnapshot = app.getState(invokeConfig);
        String nextNode = stateSnapshot.config().nextNode().orElse("");
        //还有下个节点，表示进入中断状态，等待用户输入后继续执行
        if (StringUtils.isNotBlank(nextNode) && !nextNode.equalsIgnoreCase(END)) {
            String intTip = WorkflowUtil.getHumanFeedbackTip(nextNode, wfNodes);
            //将等待输入信息[事件与提示词]发送到到客户端
            SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_WAIT_FEEDBACK_BY_" + nextNode + "]", intTip);
            InterruptedFlow.put(wfState.getUuid(), this);
            //更新状态
            wfState.setProcessStatus(WORKFLOW_PROCESS_STATUS_WAITING_INPUT);
            workflowRuntimeService.updateOutput(wfRuntimeResp.getId(), wfState);
        } else {
            WorkflowRuntime updatedRuntime = workflowRuntimeService.updateOutput(wfRuntimeResp.getId(), wfState);
            sseEmitterHelper.sendComplete(user.getId(), sseEmitter, JsonUtil.toJson(updatedRuntime.getOutput()));
            InterruptedFlow.remove(wfState.getUuid());
        }
    }

    /**
     * 中断流程等待用户输入时，会进行暂停状态，用户输入后调用本方法执行流程剩余部分
     *
     * @param userInput 用户输入
     */
    public void resume(String userInput) {
        RunnableConfig invokeConfig = RunnableConfig.builder().build();
        try {
            app.updateState(invokeConfig, Map.of(HUMAN_FEEDBACK_KEY, userInput), null);
            exe(invokeConfig, true);
        } catch (Throwable e) {
            errorWhenExe(e instanceof Exception ? (Exception) e : new RuntimeException(e));
        } finally {
            //有可能多次接收人机交互，待整个流程完全执行后才能删除
            if (wfState.getProcessStatus() != WORKFLOW_PROCESS_STATUS_WAITING_INPUT) {
                InterruptedFlow.remove(wfState.getUuid());
            }
        }
    }

    /**
     * Blocking execution: build the workflow graph, run it synchronously,
     * and return the final output as a JSON response.
     */
    public ResponseEntity<Map<String, Object>> blockingRun(User user, List<ObjectNode> userInputs) {
        this.user = user;
        // Create a dummy SseEmitter and mark it completed via sendComplete,
        // so all SSE send operations in runNode silently skip (COMPLETED_SSE cache check)
        this.sseEmitter = new SseEmitter(0L);
        sseEmitterHelper.sendComplete(user.getId(), this.sseEmitter);
        log.info("WorkflowEngine blockingRun,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflow.getUuid(), userInputs);
        if (!this.workflow.getIsEnable()) {
            throw new BaseException(ErrorEnum.A_WF_DISABLED);
        }

        Long workflowId = this.workflow.getId();
        this.wfRuntimeResp = workflowRuntimeService.create(user, workflowId);

        String runtimeUuid = this.wfRuntimeResp.getUuid();
        try {
            Pair<WorkflowNode, Set<WorkflowNode>> startAndEnds = findStartAndEndNode();
            WorkflowNode startNode = startAndEnds.getLeft();
            List<NodeIOData> wfInputs = getAndCheckUserInput(userInputs, startNode);
            this.wfState = new WfState(user, wfInputs, runtimeUuid);
            if (!wfState.getInterruptNodes().isEmpty()) {
                throw new BaseException(ErrorEnum.A_PARAMS_ERROR);
            }
            workflowRuntimeService.updateInput(this.wfRuntimeResp.getId(), wfState);

            CompileNode rootCompileNode = new CompileNode();
            rootCompileNode.setId(startNode.getUuid());

            Map<String, Integer> nodeVisitCount = new HashMap<>();
            buildCompileNode(rootCompileNode, startNode, nodeVisitCount);

            StateGraph<WfNodeState> mainStateGraph = new StateGraph<>(stateSerializer);
            this.wfState.addEdge(START, startNode.getUuid());
            buildStateGraph(null, mainStateGraph, rootCompileNode);

            MemorySaver saver = new MemorySaver();
            CompileConfig compileConfig = CompileConfig.builder()
                    .checkpointSaver(saver)
                    .build();
            app = mainStateGraph.compile(compileConfig);
            RunnableConfig invokeConfig = RunnableConfig.builder().build();

            // Synchronous invoke instead of streaming
            app.invoke(Map.of(), invokeConfig);

            // Collect results from completed nodes
            WorkflowRuntime updatedRuntime = workflowRuntimeService.updateOutput(wfRuntimeResp.getId(), wfState);

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("success", true);
            Map<String, Object> data = new LinkedHashMap<>();
            data.put("task_id", runtimeUuid);
            data.put("status", "completed");
            if (null != updatedRuntime) {
                data.put("outputs", updatedRuntime.getOutput());
            }
            result.put("data", data);
            return ResponseEntity.ok(result);
        } catch (Throwable e) {
            log.error("blockingRun execution exception, workflowUuid:{}", workflow.getUuid(), e);
            String errorMsg = e.getMessage();
            workflowRuntimeService.updateStatus(wfRuntimeResp.getId(), WORKFLOW_PROCESS_STATUS_FAIL, errorMsg);

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("success", false);
            result.put("message", errorMsg);
            return ResponseEntity.internalServerError().body(result);
        }
    }

    private void errorWhenExe(Exception e) {
        log.error("error", e);
        String errorMsg = e.getMessage();
        if (errorMsg.contains("parallel node doesn't support conditional branch")) {
            errorMsg = "Parallel nodes cannot contain conditional branches";
        }
        sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, errorMsg);
        workflowRuntimeService.updateStatus(wfRuntimeResp.getId(), WORKFLOW_PROCESS_STATUS_FAIL, errorMsg);
    }

    private Map<String, Object> runNode(WorkflowNode wfNode, WfNodeState nodeState) {
        Map<String, Object> resultMap = new HashMap<>();
        try {
            WorkflowComponent wfComponent = components.stream().filter(item -> item.getId().equals(wfNode.getWorkflowComponentId())).findFirst().orElseThrow();
            AbstractWfNode abstractWfNode = WfNodeFactory.create(wfComponent, wfNode, wfState, nodeState);
            //节点实例
            WfRuntimeNodeDto runtimeNodeDto = workflowRuntimeNodeService.createByState(user, wfNode.getId(), wfRuntimeResp.getId(), nodeState);
            wfState.getRuntimeNodes().add(runtimeNodeDto);

            SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_RUN_" + wfNode.getUuid() + "]", JsonUtil.toJson(runtimeNodeDto));

            NodeProcessResult processResult = abstractWfNode.process((is) -> {
                workflowRuntimeNodeService.updateInput(runtimeNodeDto.getId(), nodeState);
                for (NodeIOData input : nodeState.getInputs()) {
                    SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_INPUT_" + wfNode.getUuid() + "]", JsonUtil.toJson(input));
                }
            }, (is) -> {
                workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), nodeState);

                //并行节点内部的节点执行结束后，需要主动向客户端发送输出结果
                String nodeUuid = wfNode.getUuid();
                List<NodeIOData> nodeOutputs = nodeState.getOutputs();
                for (NodeIOData output : nodeOutputs) {
                    log.info("callback node:{},output:{}", nodeUuid, output.getContent());
                    SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_OUTPUT_" + nodeUuid + "]", JsonUtil.toJson(output));
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
//langgraph4j state data is not stored, only metadata is stored
        //langgraph4j state中的data不做数据存储，只存储元数据
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
     * @param sseEmitter sse emitter
     */
    private void streamingResult(WfState wfState, AsyncGenerator<NodeOutput<WfNodeState>> outputs, SseEmitter sseEmitter) {
        for (NodeOutput<WfNodeState> out : outputs) {
            if (out instanceof StreamingOutput<WfNodeState> streamingOutput) {
                String node = streamingOutput.node();
                String chunk = streamingOutput.chunk();
                log.info("node:{},chunk:{}", node, streamingOutput.chunk());
                SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_CHUNK_" + node + "]", chunk);
            } else {
                AbstractWfNode abstractWfNode = wfState.getCompletedNodes().stream().filter(item -> item.getNode().getUuid().endsWith(out.node())).findFirst().orElse(null);
                if (null != abstractWfNode) {
                    WfRuntimeNodeDto runtimeNodeDto = wfState.getRuntimeNodeByNodeUuid(out.node());
                    if (null != runtimeNodeDto) {
                        workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), abstractWfNode.getState());
                        wfState.setOutput(abstractWfNode.getState().getOutputs());
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

//ConditionalEdge creation is handled separately
            //ConditionalEdge 的创建另外处理
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
            //节点是"条件分支"或"分类"的情况下不支持并行执行，所以直接使用条件ConditionalEdge
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

//Determine if node belongs to subgraph
    //判断节点是否属于子图
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

        //记录人机交互节点
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
