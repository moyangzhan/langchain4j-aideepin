package com.moyz.adi.common.workflow;

import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.service.LLMCallRecordService;
import com.moyz.adi.common.util.LLMTokenUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import com.moyz.adi.common.vo.ChatModelRequestParams;
import com.moyz.adi.common.vo.SseAskParams;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataContent;
import com.moyz.adi.common.workflow.metrics.LLMMetrics;
import com.moyz.adi.common.workflow.node.humanfeedback.HumanFeedbackNode;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.output.TokenUsage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.bsc.langgraph4j.langchain4j.generators.StreamingChatGenerator;
import org.bsc.langgraph4j.state.AgentState;

import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;

@Slf4j
public class WorkflowUtil {

    public static String renderTemplate(String template, List<NodeIOData> values) {
        String result = template;
        for (NodeIOData next : values) {
            String name = next.getName();
            NodeIODataContent<?> dataContent = next.getContent();
            if (dataContent.getType().equals(WfIODataTypeEnum.FILES.getValue())) {
                List<String> value = (List) dataContent.getValue();
                result = result.replace("{" + name + "}", String.join(",", value));
            } else if (dataContent.getType().equals(WfIODataTypeEnum.OPTIONS.getValue())) {
                Map<String, Object> value = (Map<String, Object>) dataContent.getValue();
                result = result.replace("{" + name + "}", value.toString());
            } else {
                result = result.replace("{" + name + "}", dataContent.getValue().toString());
            }
        }
        return result;
    }

    public static void streamingInvokeLLM(WfState wfState, WfNodeState state, WorkflowNode node, String modelPlatform, String modelName, List<ChatMessage> msgs) {
        log.info("stream invoke");
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(modelPlatform, modelName);
        StreamingChatGenerator<AgentState> streamingGenerator = StreamingChatGenerator.builder()
                .mapResult(response -> {
                    String responseTxt = response.aiMessage().text();
                    log.info("llm response:{}", responseTxt);
                    TokenUsage tokenUsage = response.metadata().tokenUsage();
                    LLMTokenUtil.cacheTokenUsage(llmService.getStringRedisTemplate(), wfState.getUuid(), tokenUsage);
                    //记录节点级别的 token 消耗 | Record node-level token usage
                    LLMMetrics nodeMetrics = (LLMMetrics) state.getMetrics();
                    if (tokenUsage != null) {
                        nodeMetrics.setInputTokens(tokenUsage.inputTokenCount());
                        nodeMetrics.setOutputTokens(tokenUsage.outputTokenCount());
                    }
                    nodeMetrics.setModelName(modelName);
                    nodeMetrics.setModelPlatform(modelPlatform);
                    //Save LLM call record
                    saveLLMCallRecord(wfState, node, modelPlatform, modelName, tokenUsage);
                    NodeIOData output = NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", responseTxt);
                    wfState.getNodeStateByNodeUuid(node.getUuid()).ifPresent(item -> item.getOutputs().add(output));
                    return Map.of("completeResult", response.aiMessage().text());
                })
                .startingNode(node.getUuid())
                .startingState(state)
                .build();
        StreamingChatModel streamingLLM = llmService.buildStreamingChatModel(
                ChatModelBuilderProperties
                        .builder()
                        .build()
        );
        ChatRequest request = ChatRequest.builder()
                .messages(msgs)
                .build();
        streamingLLM.chat(request, streamingGenerator.handler());
        wfState.getNodeToStreamingGenerator().put(node.getUuid(), streamingGenerator);
//LLM returned chunks are stored in blocking queue, not processed here, handled by WorkflowEngine
        //LLM返回的chunk存放在阻塞队列中，此处不做处理，交由WorkflowEngine统一处理
//            for (StreamingOutput<AgentState> r : streamingGenerator) {
//                log.info("chunk:{}", r);
//            }
//            Optional<Object> resultValue = streamingGenerator.resultValue();
//            if (resultValue.isPresent()) {
//                Map<String, String> resultMap = (Map<String, String>) resultValue.get();
//                WfNodeIODataText output = new WfNodeIODataText(DEFAULT_OUTPUT_PARAM_NAME, resultMap.get("completeResult"));
//                return List.of(output);
//            }
    }

    public static NodeIOData invokeLLM(WfState wfState, WfNodeState nodeState, String modelPlatform, String modelName, String prompt) {
        log.info("common invoke");
        AbstractLLMService llmService = LLMContext.getServiceOrDefault(modelPlatform, modelName);
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUuid(wfState.getUuid());
        sseAskParams.setHttpRequestParams(ChatModelRequestParams.builder().systemMessage(StringUtils.EMPTY).userMessage(prompt).build());
        sseAskParams.setModelName(llmService.getAiModel().getName());
        sseAskParams.setUser(wfState.getUser());
        ChatResponse response = llmService.chat(sseAskParams);
        log.info("llm response:{}", response);
        //记录节点级别的 token 消耗 | Record node-level token usage
        if (nodeState != null && response.metadata() != null) {
            TokenUsage tokenUsage = response.metadata().tokenUsage();
            LLMMetrics nodeMetrics = (LLMMetrics) nodeState.getMetrics();
            if (tokenUsage != null) {
                nodeMetrics.setInputTokens(tokenUsage.inputTokenCount());
                nodeMetrics.setOutputTokens(tokenUsage.outputTokenCount());
            }
            nodeMetrics.setModelName(modelName);
            nodeMetrics.setModelPlatform(modelPlatform);
            //Save LLM call record
            saveLLMCallRecord(wfState, null, modelPlatform, modelName, tokenUsage);
        }
        return NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", response.aiMessage().text());
    }

    public static String getHumanFeedbackTip(String nodeUuid, List<WorkflowNode> wfNodes) {
        WorkflowNode wfNode = wfNodes.stream().filter(item -> item.getUuid().equals(nodeUuid)).findFirst().orElse(null);
        if (null == wfNode) {
            return "";
        }
        return HumanFeedbackNode.getTip(wfNode);
    }

    /**
     * 异步保存 LLM 调用记录 | Save LLM call record asynchronously
     */
    private static void saveLLMCallRecord(WfState wfState, WorkflowNode node, String modelPlatform, String modelName, TokenUsage tokenUsage) {
        try {
            Long sourceId = node != null ? node.getId() : 0L;
            LLMCallRecord record = new LLMCallRecord();
            record.setUuid(UuidUtil.createShort());
            record.setSourceType(LLMCallRecordSourceType.WORKFLOW_NODE.getValue());
            record.setSourceId(sourceId);
            record.setUserId(wfState.getUser().getId());
            record.setModelPlatform(modelPlatform);
            record.setModelName(modelName);
            if (tokenUsage != null) {
                record.setInputTokens(tokenUsage.inputTokenCount());
                record.setOutputTokens(tokenUsage.outputTokenCount());
            }
            SpringUtil.getBean(LLMCallRecordService.class).saveAsync(record);
        } catch (Exception e) {
            log.error("Failed to save LLM call record for workflow node", e);
        }
    }
}
