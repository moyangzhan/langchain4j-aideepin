package com.moyz.adi.common.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.dto.workflow.WfRuntimeNodeDto;
import com.moyz.adi.common.dto.workflow.WfRuntimeResp;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.service.*;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataTextContent;
import com.moyz.adi.common.workflow.def.WfNodeIO;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.bsc.async.AsyncGenerator;
import org.bsc.langgraph4j.CompiledGraph;
import org.bsc.langgraph4j.NodeOutput;
import org.bsc.langgraph4j.StateGraph;
import org.bsc.langgraph4j.langchain4j.generators.StreamingChatGenerator;
import org.bsc.langgraph4j.serializer.std.ObjectStreamStateSerializer;
import org.bsc.langgraph4j.state.AgentState;
import org.bsc.langgraph4j.streaming.StreamingOutput;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.WORKFLOW_NODE_PROCESS_TYPE_CONDITIONAL;
import static com.moyz.adi.common.enums.ErrorEnum.*;
import static org.bsc.langgraph4j.StateGraph.END;
import static org.bsc.langgraph4j.StateGraph.START;
import static org.bsc.langgraph4j.action.AsyncEdgeAction.edge_async;
import static org.bsc.langgraph4j.action.AsyncNodeAction.node_async;

@Slf4j
@Component
public class WorkflowEngine {

    @Lazy
    @Resource
    private WorkflowEngine self;

    @Resource
    private WorkflowService workflowService;

    @Resource
    private WorkflowNodeService workflowNodeService;

    @Resource
    private WorkflowEdgeService workflowEdgeService;

    @Resource
    private WorkflowComponentService workflowComponentService;

    @Resource
    private WorkflowRuntimeService workflowRuntimeService;

    @Resource
    private WorkflowRuntimeNodeService workflowRuntimeNodeService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;

    private final ObjectStreamStateSerializer<WfNodeState> stateSerializer = new ObjectStreamStateSerializer<>(WfNodeState::new);

    public SseEmitter streaming(User user, String workflowUuid, List<ObjectNode> userInputs) {
        SseEmitter sseEmitter = new SseEmitter();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        self.asyncRun(user, workflowUuid, userInputs, sseEmitter);
        return sseEmitter;
    }

    @Async
    public void asyncRun(User user, String workflowUuid, List<ObjectNode> userInputs, SseEmitter sseEmitter) {
        log.info("WorkflowEngine run,userId:{},workflowUuid:{},userInputs:{}", user.getId(), workflowUuid, userInputs);
        Workflow workflow = workflowService.getByUuid(workflowUuid);
        if (null == workflow) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, ErrorEnum.A_WF_NOT_FOUND.getInfo());
            throw new BaseException(ErrorEnum.A_WF_NOT_FOUND);
        } else if (!workflow.getIsEnable()) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, ErrorEnum.A_WF_DISABLED.getInfo());
            throw new BaseException(ErrorEnum.A_WF_DISABLED);
        }

        Long workflowId = workflow.getId();
        WfRuntimeResp wfRuntime = workflowRuntimeService.create(user, workflowId);
        sseEmitterHelper.startSse(user, sseEmitter, JsonUtil.toJson(wfRuntime));

        List<WorkflowComponent> components = workflowComponentService.getAllEnable();
        List<WorkflowNode> nodes = workflowNodeService.lambdaQuery()
                .eq(WorkflowNode::getWorkflowId, workflowId)
                .eq(WorkflowNode::getIsDeleted, false)
                .list();
        List<WorkflowEdge> edges = workflowEdgeService.lambdaQuery()
                .eq(WorkflowEdge::getWorkflowId, workflowId)
                .eq(WorkflowEdge::getIsDeleted, false)
                .list();

        //TODO... Check for circular references
        try {
            Pair<WorkflowNode, Set<WorkflowNode>> startAndEnds = findStartAndEndNode(nodes, edges, components);
            WorkflowNode startNode = startAndEnds.getLeft();
            Set<WorkflowNode> endNodes = startAndEnds.getRight();
            List<NodeIOData> wfInputs = getAndCheckUserInput(userInputs, startNode);
            //工作流运行实例状态
            WfState wfRuntimeState = new WfState(user, wfInputs, wfRuntime.getUuid());
            workflowRuntimeService.updateInput(wfRuntime.getId(), wfRuntimeState);
            StateGraph<WfNodeState> stateGraph = new StateGraph<>(stateSerializer);
            //Init nodes
            for (WorkflowNode wfNode : nodes) {
                WorkflowComponent wfComponent = components.stream()
                        .filter(component -> component.getId().equals(wfNode.getWorkflowComponentId()))
                        .findFirst()
                        .orElseThrow(() -> new BaseException(ErrorEnum.B_WF_NODE_DEFINITION_NOT_FOUND));
                stateGraph.addNode(wfNode.getUuid(), node_async(state -> {
                    Map<String, Object> resultMap = new HashMap<>();
                    try {
                        AbstractWfNode abstractWfNode = WfNodeFactory.create(wfComponent, wfNode, wfRuntimeState, state);
                        //节点实例
                        WfRuntimeNodeDto runtimeNodeDto = workflowRuntimeNodeService.createByState(user, wfNode.getId(), wfRuntime.getId(), state);
                        wfRuntimeState.getRuntimeNodes().add(runtimeNodeDto);

                        SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_RUN_" + wfNode.getUuid() + "]", JsonUtil.toJson(runtimeNodeDto));

                        NodeProcessResult processResult = abstractWfNode.process((is) -> {
                            workflowRuntimeNodeService.updateInput(runtimeNodeDto.getId(), state);
                            for (NodeIOData input : state.getInputs()) {
                                SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_INPUT_" + wfNode.getUuid() + "]", JsonUtil.toJson(input));
                            }
                        }, (is) -> workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), state));
                        if (StringUtils.isNotBlank(processResult.getNextNodeUuid())) {
                            resultMap.put("next", processResult.getNextNodeUuid());
                        }
                    } catch (Exception e) {
                        log.error("Node run error", e);
                        throw new BaseException(ErrorEnum.B_WF_RUN_ERROR);
                    }
                    resultMap.put("name", wfNode.getTitle());
                    //langgraph4j state中的data不做数据存储，只存储元数据
                    StreamingChatGenerator<AgentState> generator = wfRuntimeState.getNodeToStreamingGenerator().get(wfNode.getUuid());
                    if (null != generator) {
                        resultMap.put("_streaming_messages", generator);
                        return resultMap;
                    }
                    return resultMap;
                }));
            }

            //处理按条件才能执行的流程
            wfRuntimeState.addEdge(START, startNode.getUuid());
            for (WorkflowEdge edgeDef : edges) {
                if (StringUtils.isNotBlank(edgeDef.getSourceHandle())) {
                    wfRuntimeState.addConditionalEdge(edgeDef.getSourceNodeUuid(), edgeDef.getTargetNodeUuid());
                } else {
                    wfRuntimeState.addEdge(edgeDef.getSourceNodeUuid(), edgeDef.getTargetNodeUuid());
                }
            }
            for (WorkflowNode endNode : endNodes) {
                wfRuntimeState.addEdge(endNode.getUuid(), END);
            }

            //Render langgraph4j edge
            for (Map.Entry<String, List<String>> entry : wfRuntimeState.getEdges().entrySet()) {
                if (entry.getValue().size() == 1) {
                    stateGraph.addEdge(entry.getKey(), entry.getValue().get(0));
                } else if (entry.getValue().size() > 1) {
                    Map<String, String> mappings = new HashMap<>();
                    for (String target : entry.getValue()) {
                        mappings.put(target, target);
                    }
                    stateGraph.addConditionalEdges(
                            entry.getKey(),
                            edge_async(state -> state.data().get("next").toString()),
                            mappings
                    );
                } else {
                    log.warn("Can not find target node,sourceNode:{}", entry.getKey());
                }
            }
            CompiledGraph<WfNodeState> app = stateGraph.compile();
            //不使用langgraph4j state的update相关方法，无需传入input
            AsyncGenerator<NodeOutput<WfNodeState>> outputs = app.stream(Map.of());
            streamingResult(wfRuntimeState, outputs, sseEmitter);
            //fillResult(outputs) //非流式返回
            WorkflowRuntime updatedRuntime = workflowRuntimeService.updateOutput(wfRuntime.getId(), wfRuntimeState);
            sseEmitterHelper.sendComplete(user.getId(), sseEmitter, JsonUtil.toJson(updatedRuntime.getOutput()));
        } catch (Exception e) {
            log.error("error", e);
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, e.getMessage());
        }
    }

    /**
     * 流式输出结果
     *
     * @param outputs    输出
     * @param sseEmitter sse emitter
     */
    private void streamingResult(WfState wfState, AsyncGenerator<NodeOutput<WfNodeState>> outputs, SseEmitter sseEmitter) throws Exception {
        for (NodeOutput<WfNodeState> out : outputs) {
            if (out instanceof StreamingOutput<WfNodeState> streamingOutput) {
                String node = streamingOutput.node();
                String chunk = streamingOutput.chunk();
                log.info("node:{},chunk:{}", node, streamingOutput.chunk());
                SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_CHUNK_" + node + "]", chunk);
            } else {
                AbstractWfNode abstractWfNode = wfState.getCompletedNodes().stream().filter(item -> item.getNode().getUuid().endsWith(out.node())).findFirst().orElse(null);
                if (null != abstractWfNode) {
                    WfNodeState nodeState = abstractWfNode.getState();
                    List<NodeIOData> nodeOutputs = nodeState.getOutputs();
                    for (NodeIOData output : nodeOutputs) {
                        log.info("node:{},output:{}", out.node(), output.getContent());
                        SSEEmitterHelper.parseAndSendPartialMsg(sseEmitter, "[NODE_OUTPUT_" + out.node() + "]", JsonUtil.toJson(output));
                    }

                    WfRuntimeNodeDto runtimeNodeDto = wfState.getRuntimeNodeByNodeUuid(out.node());
                    if (null != runtimeNodeDto) {
                        workflowRuntimeNodeService.updateOutput(runtimeNodeDto.getId(), abstractWfNode.getState());
                        wfState.setOutput(abstractWfNode.getState().getOutputs());
                    }

                } else {
                    log.warn("Can not find node state,node uuid:{}", out.node());
                }
            }
        }
    }

    /**
     * 填充返回结果（非流式输出的情况下）
     *
     * @param outputs 节点的输出
     * @return 最终返回给用户的结果
     */
    private Map<String, List<NodeIOData>> fillResult(AsyncGenerator<NodeOutput<WfNodeState>> outputs) {
        Map<String, List<NodeIOData>> result = new HashMap<>();
        for (NodeOutput<WfNodeState> out : outputs) {
            if (out instanceof StreamingOutput<WfNodeState> streamingOutput) {
                log.info("StreamingOutput{node={}, chunk={} }", streamingOutput.node(), streamingOutput.chunk());
                List<NodeIOData> llmOutput = result.getOrDefault(out.node(), new ArrayList<>());
                if (llmOutput.isEmpty()) {
                    llmOutput.add(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", ""));
                }
                NodeIOData first = llmOutput.get(0);
                if (first.getContent() instanceof NodeIODataTextContent textContent) {
                    textContent.setValue(textContent.getValue() + streamingOutput.chunk());
                }
            } else {
                log.info("common output:{}", out);
                result.put(out.node(), out.state().getOutputs());
            }
        }
        return result;
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
                    log.error("用户输入无效,workflowId:{}", startNode.getWorkflowId());
                    throw new BaseException(ErrorEnum.A_WF_INPUT_INVALID);
                }
                wfInputs.add(nodeIOData);
            }
            if (requiredParamMissing) {
                log.error("在流程定义中必填的参数没有传进来,name:{}", paramNameFromDef);
                throw new BaseException(A_WF_INPUT_MISSING);
            }
        }
        return wfInputs;
    }

    /**
     * 查找开始及结束节点 <br/>
     * 开始节点只能有一个，结束节点可能多个
     *
     * @param nodes      节点定义
     * @param edges      边定义
     * @param components 组件
     * @return 开始节点及结束节点列表
     */
    public Pair<WorkflowNode, Set<WorkflowNode>> findStartAndEndNode(List<WorkflowNode> nodes, List<WorkflowEdge> edges, List<WorkflowComponent> components) {
        WorkflowNode startNode = null;
        Set<WorkflowNode> endNodes = new HashSet<>();
        for (WorkflowNode node : nodes) {
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
            log.error("没有开始节点,workflowId:{}", nodes.get(0).getWorkflowId());
            throw new BaseException(ErrorEnum.A_WF_START_NODE_NOT_FOUND);
        }
        //Find all end nodes
        nodes.forEach(item -> {
            String nodeUuid = item.getUuid();
            boolean source = false;
            boolean target = false;
            for (WorkflowEdge edgeDef : edges) {
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
            log.error("没有结束节点,workflowId:{}", startNode.getWorkflowId());
            throw new BaseException(A_WF_END_NODE_NOT_FOUND);
        }
        return Pair.of(startNode, endNodes);
    }

    public void stop() {
        System.out.println("WorkflowEngine stop");
    }
}
