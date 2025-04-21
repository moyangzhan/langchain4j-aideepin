package com.moyz.adi.common.workflow;

import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.interfaces.AbstractLLMService;
import com.moyz.adi.common.util.LLMTokenUtil;
import com.moyz.adi.common.vo.AssistantChatParams;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import com.moyz.adi.common.vo.SseAskParams;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataContent;
import com.moyz.adi.common.workflow.node.humanfeedback.HumanFeedbackNode;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.chat.request.ChatRequest;
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

    public static void streamingInvokeLLM(WfState wfState, WfNodeState state, WorkflowNode node, String modelName, List<ChatMessage> msgs) {
        log.info("stream invoke");
        AbstractLLMService<?> llmService = LLMContext.getLLMServiceByName(modelName);
        StreamingChatGenerator<AgentState> streamingGenerator = StreamingChatGenerator.builder()
                .mapResult(response -> {
                    String responseTxt = response.aiMessage().text();
                    log.info("llm response:{}", responseTxt);
                    LLMTokenUtil.cacheTokenUsage(llmService.getStringRedisTemplate(), wfState.getUuid(), response.metadata().tokenUsage());
                    NodeIOData output = NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", responseTxt);
                    wfState.getNodeStateByNodeUuid(node.getUuid()).ifPresent(item -> item.getOutputs().add(output));
                    return Map.of("completeResult", response.aiMessage().text());
                })
                .startingNode(node.getUuid())
                .startingState(state)
                .build();
        StreamingChatLanguageModel streamingLLM = llmService.buildStreamingChatLLM(
                LLMBuilderProperties
                        .builder()
                        .build()
        );
        ChatRequest request = ChatRequest.builder()
                .messages(msgs)
                .build();
        streamingLLM.chat(request, streamingGenerator.handler());
        wfState.getNodeToStreamingGenerator().put(node.getUuid(), streamingGenerator);
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

    public static NodeIOData invokeLLM(WfState wfState, String modelName, String prompt) {
        log.info("common invoke");
        AbstractLLMService<?> llmService = LLMContext.getLLMServiceByName(modelName);
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUuid(wfState.getUuid());
        sseAskParams.setAssistantChatParams(AssistantChatParams.builder().systemMessage(StringUtils.EMPTY).userMessage(prompt).build());
        sseAskParams.setModelName(llmService.getAiModel().getName());
        sseAskParams.setUser(wfState.getUser());
        String response = llmService.chat(sseAskParams);
        log.info("llm response:{}", response);
        return NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", response);
    }

    public static String getHumanFeedbackTip(String nodeUuid, List<WorkflowNode> wfNodes) {
        WorkflowNode wfNode = wfNodes.stream().filter(item -> item.getUuid().equals(nodeUuid)).findFirst().orElse(null);
        if (null == wfNode) {
            return "";
        }
        return HumanFeedbackNode.getTip(wfNode);
    }
}
