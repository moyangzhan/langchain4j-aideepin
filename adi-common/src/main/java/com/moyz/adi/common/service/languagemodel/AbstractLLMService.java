package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.helper.TtsModelContext;
import com.moyz.adi.common.interfaces.TriConsumer;
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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.LLM_MAX_INPUT_TOKENS_DEFAULT;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_LLM_SERVICE_DISABLED;

@Slf4j
public abstract class AbstractLLMService<T> extends CommonModelService<T> {

    protected StringRedisTemplate stringRedisTemplate;

    //TTS相关部分参数
    private String ttsJobId;
    private TtsModelContext ttsModelContext;
    private String ttsJobFilePath;
    private TtsSetting ttsSetting;

    protected AbstractLLMService(AiModel aiModel, String settingName, Class<T> clazz) {
        super(aiModel, settingName, clazz);

        initMaxInputTokens();
        ttsSetting = JsonUtil.fromJson(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.TTS_SETTING), TtsSetting.class);
        if (null == ttsSetting) {
            log.error("TTS配置未找到，请检查配置文件，请检查 adi_sys_config 中是否有 tts_setting 配置项");
            throw new BaseException(ErrorEnum.B_TTS_SETTING_NOT_FOUND);
        }
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
    public void streamingChat(SseAskParams params, TriConsumer<LLMResponseContent, PromptMeta, AnswerMeta> consumer) {
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
                .answerContentType(params.getAnswerContentType())
                .consumer(consumer)
                .build();
        try {

            //如果系统设置的语音合成器类型是后端合成，并且设置的返回内容是音频，则初始化tts任务并注册回调函数
            if (AdiConstant.TtsConstant.SYNTHESIZER_SERVER.equals(ttsSetting.getSynthesizerSide()) && params.getAnswerContentType() == AdiConstant.ConversationConstant.ANSWER_CONTENT_TYPE_AUDIO) {
                ttsJobId = UuidUtil.createShort();
                ttsModelContext = new TtsModelContext();
                ttsModelContext.startTtsJob(ttsJobId, "", (ByteBuffer audioFrame) -> {
                    byte[] frameBytes = new byte[audioFrame.remaining()];
                    audioFrame.get(frameBytes);
                    String base64Audio = Base64.getEncoder().encodeToString(frameBytes);
                    SSEEmitterHelper.sendAudio(params.getSseEmitter(), base64Audio);
                }, (String filePathOrOssUrl) -> {
                    ttsJobFilePath = filePathOrOssUrl;
                }, (String errorMsg) -> {
                    log.error("tts error: {}", errorMsg);
                });
            }

            //不管是不是需要返回音频文件，都需要innerStreamingChat()
            innerStreamingChat(innerStreamChatParams);
        } catch (Exception e) {
            //Close MCP clients
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

                if (null != ttsModelContext && AdiConstant.TtsConstant.SYNTHESIZER_SERVER.equals(ttsSetting.getSynthesizerSide()) && params.getAnswerContentType() == AdiConstant.ConversationConstant.ANSWER_CONTENT_TYPE_AUDIO) {
                    ttsModelContext.processPartialText(ttsJobId, partialResponse);
                }
            }

            @Override
            public void onCompleteResponse(ChatResponse response) {
                AiMessage responseAiMessage = response.aiMessage();
                if (responseAiMessage.hasToolExecutionRequests()) {
                    // 如果有工具执行请求
                    List<ToolExecutionResultMessage> toolExecutionMessages = createToolExecutionMessages(responseAiMessage, toolSpecificationMcpClientMap);

                    //mcp调用消息格式参考：https://docs.langchain4j.dev/tutorials/tools/
                    AiMessage aiMessage = AiMessage.aiMessage(responseAiMessage.toolExecutionRequests());
                    List<ChatMessage> messages = new ArrayList<>(params.getChatRequest().messages());
                    messages.add(aiMessage);
                    messages.addAll(toolExecutionMessages);
                    params.setChatRequest(ChatRequest.builder()
                            .messages(messages)
                            .parameters(params.getChatRequest().parameters())
                            .build());
                    // recursive call now with tool calling results
                    innerStreamingChat(params);
                } else {
                    //结束TTS任务
                    if (null != ttsModelContext) {
                        //TODO。。。 停止转换任务，此时有可能导致只合成部分音频，待处理
                        ttsModelContext.complete(ttsJobId);
                    }
                    //结束整个对话任务
                    Pair<PromptMeta, AnswerMeta> pair = SSEEmitterHelper.calculateToken(response, params.getUuid());
                    params.getConsumer().accept(new LLMResponseContent(response.aiMessage().text(), ttsJobFilePath), pair.getLeft(), pair.getRight());
                    //Close mcp clients
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
                //Close MCP clients
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
            AiMessage responseAiMessage = chatResponse.aiMessage();
            if (responseAiMessage.hasToolExecutionRequests()) {
                Map<ToolSpecification, McpClient> toolSpecificationMcpClientMap = getRequestTools(chatModelParams.getMcpClients());
                List<ToolExecutionResultMessage> toolExecutionMessages = createToolExecutionMessages(responseAiMessage, toolSpecificationMcpClientMap);

                AiMessage aiMessage = AiMessage.aiMessage(responseAiMessage.toolExecutionRequests());
                List<ChatMessage> messages = new ArrayList<>(chatRequest.messages());
                messages.add(aiMessage);
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
        } finally {
            chatModelParams.getMcpClients().forEach(item -> {
                try {
                    item.close();
                } catch (Exception e1) {
                    throw new RuntimeException(e1);
                }
            });
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

            //处理重复的UserMessage
            if (!memory.messages().isEmpty()) {
                ChatMessage lastMessage = memory.messages().get(memory.messages().size() - 1);
                if (lastMessage instanceof UserMessage) {
                    List<ChatMessage> list = memory.messages().subList(0, memory.messages().size() - 1);
                    memory.clear();
                    list.forEach(memory::add);
                }
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
