package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.rag.TokenEstimatorFactory;
import com.moyz.adi.common.rag.TokenEstimatorThreadLocal;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.agent.tool.ToolExecutionRequest;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.data.message.*;
import dev.langchain4j.mcp.McpToolProvider;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.memory.chat.TokenWindowChatMemory;
import dev.langchain4j.model.TokenCountEstimator;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.request.ChatRequestParameters;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.chat.response.StreamingChatResponseHandler;
import dev.langchain4j.service.tool.ToolProvider;
import dev.langchain4j.service.tool.ToolService;
import dev.langchain4j.service.tool.ToolServiceContext;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected InetSocketAddress proxyAddress;
    @Getter
    protected AiModel aiModel;
    protected T modelPlatformSetting;

    protected StringRedisTemplate stringRedisTemplate;

    protected AbstractLLMService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        modelPlatformSetting = JsonUtil.fromJson(st, clazz);

        initMaxInputTokens();
    }

    private void initMaxInputTokens() {
        if (this.aiModel.getMaxInputTokens() < 1) {
            this.aiModel.setMaxInputTokens(LLM_MAX_INPUT_TOKENS_DEFAULT);
        }
    }

    public StringRedisTemplate getStringRedisTemplate() {
        if (null == this.stringRedisTemplate) {
            this.stringRedisTemplate = SpringUtil.getBean(StringRedisTemplate.class);
        }
        return this.stringRedisTemplate;
    }

    public AbstractLLMService<T> setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }

    /**
     * 检测该service是否可用（不可用的情况通常是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    protected boolean checkBeforeChat(SseAskParams params) {
        return true;
    }

    public ChatModel buildChatLLM(LLMBuilderProperties properties, String uuid) {
        LLMBuilderProperties tmpProperties = properties;
        if (null == properties) {
            tmpProperties = new LLMBuilderProperties();
            tmpProperties.setTemperature(0.7);
            log.info("llmBuilderProperties is null, set default temperature:{}", tmpProperties.getTemperature());
        }
        if (null == tmpProperties.getTemperature() || tmpProperties.getTemperature() <= 0 || tmpProperties.getTemperature() > 1) {
            tmpProperties.setTemperature(0.7);
            log.info("llmBuilderProperties temperature is invalid, set default temperature:{}", tmpProperties.getTemperature());
        }
        return doBuildChatModel(tmpProperties);
    }

    protected abstract ChatModel doBuildChatModel(LLMBuilderProperties properties);

    public abstract StreamingChatModel buildStreamingChatModel(LLMBuilderProperties properties);

    protected abstract LLMException parseError(Object error);

    public abstract TokenCountEstimator getTokenEstimator();

    /**
     * 普通聊天，将原始的用户问题及历史消息发送给AI
     *
     * @param params   请求参数
     * @param consumer 响应结果回调
     */
    public void streamingChat(SseAskParams params, boolean shutdownSse, TriConsumer<String, PromptMeta, AnswerMeta> consumer) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }
        ChatModelParams chatModelParams = params.getChatModelParams();
        log.info("sseChat,messageId:{}", chatModelParams.getMemoryId());
        List<ChatMessage> chatMessages = createChatMessages(chatModelParams);
        StreamingChatModel streamingChatModel = buildStreamingChatModel(params.getLlmBuilderProperties());

        ChatRequest chatRequest = createChatRequest(chatModelParams.getMcpClients(), chatMessages);
        InnerStreamChatParams innerStreamChatParams = InnerStreamChatParams.builder()
                .uuid(params.getUuid())
                .streamingChatModel(streamingChatModel)
                .chatRequest(chatRequest)
                .sseEmitter(params.getSseEmitter())
                .mcpClients(params.getChatModelParams().getMcpClients())
                .shutdownSse(shutdownSse)
                .consumer(consumer)
                .build();
        try {
            innerStreamingChat(innerStreamChatParams);
        } catch (Exception e) {
            params.getChatModelParams().getMcpClients().forEach(item -> {
                try {
                    item.close();
                } catch (Exception e1) {
                    throw new RuntimeException(e1);
                }
            });
            throw e;
        }

    }

    /**
     * 内部流式聊天方法，处理工具调用等复杂逻辑
     *
     * @param params 参数对象，包含流式聊天所需的所有信息
     */
    private void innerStreamingChat(InnerStreamChatParams params) {
        Map<ToolSpecification, McpClient> toolSpecificationMcpClientMap = getRequestTools(params.getMcpClients());
        params.getStreamingChatModel().chat(params.getChatRequest(), new StreamingChatResponseHandler() {
            @Override
            public void onPartialResponse(String partialResponse) {
                SSEEmitterHelper.parseAndSendPartialMsg(params.getSseEmitter(), partialResponse);
            }

            @Override
            public void onCompleteResponse(ChatResponse response) {
                AiMessage aiMessage = response.aiMessage();
                if (aiMessage.hasToolExecutionRequests()) {
                    // 如果有工具执行请求
                    List<ToolExecutionResultMessage> toolExecutionMessages = createToolExecutionMessages(aiMessage, toolSpecificationMcpClientMap);
                    List<ChatMessage> messages = new ArrayList<>(params.getChatRequest().messages());
                    messages.addAll(toolExecutionMessages);
                    params.setChatRequest(ChatRequest.builder()
                            .messages(messages)
                            .parameters(params.getChatRequest().parameters())
                            .build());
                    // recursive call now with tool calling results
                    innerStreamingChat(params);
                } else {
                    Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateTokenAndShutdown(response, params.getSseEmitter(), params.getUuid(), params.isShutdownSse());
                    params.getConsumer().accept(response.aiMessage().text(), pair.getLeft(), pair.getRight());
                    params.getMcpClients().forEach(item -> {
                        try {
                            item.close();
                        } catch (Exception e) {
                            throw new RuntimeException(e);
                        }
                    });
                }
            }

            @Override
            public void onError(Throwable error) {
                SSEEmitterHelper.errorAndShutdown(error, params.getSseEmitter());
                params.getMcpClients().forEach(item -> {
                    try {
                        item.close();
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                });
            }
        });
    }

    public ChatResponse chat(SseAskParams params) {
        if (!isEnabled()) {
            log.error("llm service is disabled");
            throw new BaseException(B_LLM_SERVICE_DISABLED);
        }
        if (!checkBeforeChat(params)) {
            log.error("对话参数校验不通过");
            throw new BaseException(A_PARAMS_ERROR);
        }

        ChatModelParams chatModelParams = params.getChatModelParams();
        ChatModel chatModel = buildChatLLM(params.getLlmBuilderProperties(), params.getUuid());
        List<ChatMessage> chatMessages = createChatMessages(chatModelParams);
        ChatRequest chatRequest = createChatRequest(chatModelParams.getMcpClients(), chatMessages);

        ChatResponse chatResponse = chatModel.chat(chatRequest);
        if (chatResponse.aiMessage().hasToolExecutionRequests()) {
            return innerChat(params.getUuid(), chatModel, chatModelParams, chatRequest);
        }

        cacheTokenUsage(params.getUuid(), chatResponse);
        return chatResponse;
    }

    /**
     * 程序内部调用的聊天方法，通常用于处理工具调用等复杂逻辑
     *
     * @param uuid            唯一标识
     * @param chatModel       聊天模型
     * @param chatModelParams 聊天模型参数
     * @param chatRequest     聊天请求
     * @return ChatResponse 聊天响应
     */
    private ChatResponse innerChat(String uuid, ChatModel chatModel, ChatModelParams chatModelParams, ChatRequest chatRequest) {
        try {
            ChatResponse chatResponse = chatModel.chat(chatRequest);
            if (chatResponse.aiMessage().hasToolExecutionRequests()) {
                Map<ToolSpecification, McpClient> toolSpecificationMcpClientMap = getRequestTools(chatModelParams.getMcpClients());
                List<ToolExecutionResultMessage> toolExecutionMessages = createToolExecutionMessages(chatResponse.aiMessage(), toolSpecificationMcpClientMap);
                List<ChatMessage> messages = new ArrayList<>(chatRequest.messages());
                messages.addAll(toolExecutionMessages);

                cacheTokenUsage(uuid, chatResponse);

                // recursive call now with tool calling results
                return innerChat(uuid, chatModel, chatModelParams, ChatRequest.builder()
                        .messages(messages)
                        .parameters(chatRequest.parameters())
                        .build());
            }
            cacheTokenUsage(uuid, chatResponse);
            return chatResponse;
        } catch (Exception e) {
            chatModelParams.getMcpClients().forEach(item -> {
                try {
                    item.close();
                } catch (Exception e1) {
                    throw new RuntimeException(e1);
                }
            });
            throw e;
        }
    }

    /**
     * 缓存token使用情况
     *
     * @param uuid         唯一标识
     * @param chatResponse 聊天响应
     */
    private void cacheTokenUsage(String uuid, ChatResponse chatResponse) {
        int inputTokenCount = chatResponse.metadata().tokenUsage().inputTokenCount();
        int outputTokenCount = chatResponse.metadata().tokenUsage().outputTokenCount();
        log.info("ChatModel token cost,uuid:{},inputTokenCount:{},outputTokenCount:{}", uuid, inputTokenCount, outputTokenCount);
        LLMTokenUtil.cacheTokenUsage(getStringRedisTemplate(), uuid, chatResponse.metadata().tokenUsage());
    }


    private List<ChatMessage> createChatMessages(ChatModelParams chatModelParams) {
        String memoryId = chatModelParams.getMemoryId();
        List<Content> userContents = new ArrayList<>();
        userContents.add(TextContent.from(chatModelParams.getUserMessage()));
        List<ChatMessage> chatMessages = new ArrayList<>();
        if (StringUtils.isNotBlank(memoryId)) {

            TokenCountEstimator tokenCountEstimator;
            String tokenEstimatorName = TokenEstimatorThreadLocal.getTokenEstimator();
            if (StringUtils.isBlank(tokenEstimatorName) && null != getTokenEstimator()) {
                tokenCountEstimator = getTokenEstimator();
            } else {
                tokenCountEstimator = TokenEstimatorFactory.create(tokenEstimatorName);
            }

            //滑动窗口算法限制消息长度
            TokenWindowChatMemory memory = TokenWindowChatMemory.builder()
                    .chatMemoryStore(MapDBChatMemoryStore.getSingleton())
                    .id(memoryId)
                    .maxTokens(aiModel.getMaxInputTokens(), tokenCountEstimator)
                    .build();
            if (StringUtils.isNotBlank(chatModelParams.getSystemMessage())) {
                memory.add(SystemMessage.from(chatModelParams.getSystemMessage()));
            }
            memory.add(UserMessage.from(userContents));

            //得到截断后符合maxTokens的文本消息
            chatMessages.addAll(memory.messages());

            //AI services currently do not support multimodality, use the low-level API for this. https://docs.langchain4j.dev/tutorials/ai-services#multimodality
            //重新组装用户消息及追加图片消息到chatMessage
            List<Content> imageContents = ImageUtil.urlsToImageContent(chatModelParams.getImageUrls());
            if (CollectionUtils.isNotEmpty(imageContents)) {
                int lastIndex = chatMessages.size() - 1;
                UserMessage lastMessage = (UserMessage) chatMessages.get(lastIndex);
                chatMessages.remove(lastIndex);
                List<Content> userMessage = new ArrayList<>();
                userMessage.addAll(lastMessage.contents());
                userMessage.addAll(imageContents);
                chatMessages.add(UserMessage.from(userMessage));
            }
            return chatMessages;
        } else {
            if (StringUtils.isNotBlank(chatModelParams.getSystemMessage())) {
                chatMessages.add(SystemMessage.from(chatModelParams.getSystemMessage()));
            }
            chatMessages.add(UserMessage.from(userContents));
        }
        return chatMessages;
    }

    private Map<ToolSpecification, McpClient> getRequestTools(List<McpClient> mcpClients) {
        Map<ToolSpecification, McpClient> tools = new HashMap<>();
        // MCP Tools
        for (McpClient mcpClient : mcpClients) {
            for (ToolSpecification toolSpecification : mcpClient.listTools()) {
                tools.put(toolSpecification, mcpClient);
            }
        }
        // native tools
//        chatRequest.tools().forEach(tool -> {
//            ToolSpecifications.toolSpecificationsFrom(tool)
//                    .forEach(spec -> tools.put(spec,
//                            (req, mem) -> new DefaultToolExecutor(tool, req).execute(req, mem)));
//        });
        return tools;
    }

    private ChatRequest createChatRequest(List<McpClient> mcpClients, List<ChatMessage> chatMessages) {
        ToolProvider toolProvider = McpToolProvider.builder()
                .mcpClients(mcpClients)
                .build();
        ToolService toolService = new ToolService();
        toolService.toolProvider(toolProvider);

        ToolServiceContext toolServiceContext = toolService.createContext(UuidUtil.createShort(), ((UserMessage) chatMessages.get(chatMessages.size() - 1)));
        log.info("tool specs:{}", toolServiceContext.toolSpecifications());
        ChatRequestParameters parameters = ChatRequestParameters.builder()
                .toolSpecifications(toolServiceContext.toolSpecifications())
                .build();

        return ChatRequest.builder()
                .messages(chatMessages)
                .parameters(parameters)
                .build();
    }

    private List<ToolExecutionResultMessage> createToolExecutionMessages(AiMessage aiMessage, Map<ToolSpecification, McpClient> toolSpecificationMcpClientMap) {
        List<ToolExecutionResultMessage> toolExecutionMessages = new ArrayList<>();
        aiMessage.toolExecutionRequests().forEach(req -> {
            log.warn("tool exec request:{},", req);
            //部分模型（如硅基流动）返回的工具请求可能没有id和name，需要手动解析，如 ToolExecutionRequest { id = "", name = "", arguments = "maps_weather
            //{"city": "广州"}" }
            if (StringUtils.isBlank(req.id())) {
                String arguments = req.arguments();
                String name = req.name();
                if (StringUtils.isBlank(name) && StringUtils.isNotBlank(arguments) && !arguments.startsWith("{")) {
                    // 如果arguments是json格式，则使用工具名称作为id
                    String[] args = arguments.split(" ");
                    if (args.length > 1) {
                        name = args[0];
                        arguments = arguments.substring(name.length()).trim();
                    } else {
                        name = "name_" + UuidUtil.createShort();
                    }
                }
                req = ToolExecutionRequest.builder().id("id_" + UuidUtil.createShort()).name(name).arguments(arguments).build();
            }
            McpClient selectedMcpClient = null;
            for (Map.Entry<ToolSpecification, McpClient> entry : toolSpecificationMcpClientMap.entrySet()) {
                if (entry.getKey().name().equals(req.name())) {
                    selectedMcpClient = entry.getValue();
                }
            }
            if (null == selectedMcpClient) {
                toolExecutionMessages.add(ToolExecutionResultMessage.from(req,
                        "No Tool executor found for this tool request"));
                return;
            }
            try {
                final String result = selectedMcpClient.executeTool(req);
                log.info("tool execute result:{}", result);
                toolExecutionMessages.add(ToolExecutionResultMessage.from(req, result));
            } catch (Exception e) {
                log.debug("Error executing tool " + req, e);
                toolExecutionMessages.add(ToolExecutionResultMessage.from(req, e.getMessage()));
            }
        });
        return toolExecutionMessages;
    }
}
