package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.enums.ChatMessageRoleEnum;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.file.LocalFileUtil;
import com.moyz.adi.common.helper.AsrModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.languagemodel.data.LLMResponseContent;
import com.moyz.adi.common.mapper.CharacterMessageMapper;
import com.moyz.adi.common.memory.longterm.LongTermMemoryService;
import com.moyz.adi.common.memory.shortterm.MapDBChatMemoryStore;
import com.moyz.adi.common.rag.AdiEmbeddingStoreContentRetriever;
import com.moyz.adi.common.rag.CompositeRag;
import com.moyz.adi.common.rag.GraphStoreContentRetriever;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.mcp.client.McpClient;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.query.Query;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import dev.langchain4j.store.embedding.filter.comparison.IsIn;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import ws.schild.jave.info.MultimediaInfo;

import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.CHARACTER_ID;
import static com.moyz.adi.common.cosntant.AdiConstant.MetadataKey.KB_UUID;
import static com.moyz.adi.common.enums.ErrorEnum.A_CHARACTER_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;
import static com.moyz.adi.common.util.AdiStringUtil.stringToList;

@Slf4j
@Service
public class CharacterMessageService extends ServiceImpl<CharacterMessageMapper, CharacterMessage> {

    @Lazy
    @Resource
    private CharacterMessageService self;

    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Lazy
    @Resource
    private CharacterService characterService;

    @Resource
    private CharacterMessageRefEmbeddingService characterMessageRefEmbeddingService;

    @Resource
    private CharacterMessageRefGraphService characterMessageRefGraphService;

    @Resource
    private CharacterMessageRefMemoryEmbeddingService characterMessageRefMemoryEmbeddingService;

    @Resource
    private UserMcpService userMcpService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;

    @Resource
    private FileService fileService;

    @Resource
    private AsyncTaskExecutor mainExecutor;

    @Resource
    private LongTermMemoryService longTermMemoryService;

    public SseEmitter sseAsk(AskReq askReq) {
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        User user = ThreadContext.getCurrentUser();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        sseEmitterHelper.startSse(user, sseEmitter);
        self.asyncCheckAndChat(sseEmitter, user, askReq);
        return sseEmitter;
    }

    /**
     * Blocking chat: same preparation as SSE but calls ChatModel.chat() instead of streaming.
     * Returns the ChatResponse after persisting messages to DB.
     */
    public ResponseEntity<Map<String, Object>> blockingAsk(AskReq askReq) {
        User user = ThreadContext.getExistCurrentUser();

        // Validate character exists
        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, askReq.getCharacterUuid())
                .eq(Character::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_CHARACTER_NOT_FOUND));

        // Quota check (same as checkCharacter's check 3)
        AiModel aiModel = LLMContext.getAiModel(askReq.getModelPlatform(), askReq.getModelName());
        if (null != aiModel && !aiModel.getIsFree()) {
            ErrorEnum quotaError = quotaHelper.checkTextQuota(user);
            if (null != quotaError) {
                throw new BaseException(quotaError);
            }
        }

        AbstractLLMService llmService = LLMContext.getServiceOrDefault(askReq.getModelPlatform(), askReq.getModelName());

        // Knowledge base retrieval
        List<KbInfoResp> filteredKb = new ArrayList<>();
        if (StringUtils.isNotBlank(character.getKbIds())) {
            List<Long> kbIds = Arrays.stream(character.getKbIds().split(",")).map(Long::parseLong).toList();
            filteredKb = characterService.filterEnableKb(user, kbIds);
        }
        List<RetrieverWrapper> retrieverWrappers = retrieve(character.getId(), filteredKb, llmService, askReq);

        // Build prompt with memory and knowledge context
        Pair<String, String> memoryAndKnowledge = buildMemoryAndKnowledge(retrieverWrappers);
        String effectiveLocale = StringUtils.isNotBlank(user.getLocale())
                ? user.getLocale()
                : Objects.toString(SysConfigService.getByKey(AdiConstant.SysConfigKey.DEFAULT_LOCALE), "zh-CN");
        String processedPrompt = PromptUtil.createPrompt(askReq.getPrompt(), memoryAndKnowledge.getLeft(), memoryAndKnowledge.getRight(), "", effectiveLocale);
        if (!Objects.equals(askReq.getPrompt(), processedPrompt)) {
            askReq.setProcessedPrompt(processedPrompt);
        }

        String questionUuid = UuidUtil.createShort();
        ChatModelRequestParams chatRequestParams = buildChatRequestParams(character, askReq);
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUser(user);
        sseAskParams.setUuid(questionUuid);
        sseAskParams.setModelName(askReq.getModelName());
        sseAskParams.setHttpRequestParams(chatRequestParams);
        sseAskParams.setModelProperties(
                ChatModelBuilderProperties.builder()
                        .temperature(character.getLlmTemperature())
                        .returnThinking(chatRequestParams.getReturnThinking())
                        .build()
        );

        // Blocking LLM call
        ChatResponse chatResponse = llmService.chat(sseAskParams);

        // Persist messages to DB
        int inputTokens = 0;
        int outputTokens = 0;
        int totalTokens = 0;
        if (chatResponse.metadata() != null && chatResponse.metadata().tokenUsage() != null) {
            inputTokens = chatResponse.metadata().tokenUsage().inputTokenCount();
            outputTokens = chatResponse.metadata().tokenUsage().outputTokenCount();
            totalTokens = chatResponse.metadata().tokenUsage().totalTokenCount();
        }
        PromptMeta questionMeta = new PromptMeta(inputTokens, questionUuid);
        AnswerMeta answerMeta = AnswerMeta.builder()
                .tokens(outputTokens)
                .uuid(UuidUtil.createShort())
                .build();
        LLMResponseContent responseContent = new LLMResponseContent(
                chatResponse.aiMessage().thinking(), chatResponse.aiMessage().text(), null);
        self.saveAfterAiResponse(user, askReq, retrieverWrappers, responseContent, questionMeta, answerMeta, null);

        // Build response
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("success", true);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("message_id", questionUuid);
        data.put("answer", chatResponse.aiMessage().text());
        Map<String, Object> usage = new LinkedHashMap<>();
        usage.put("prompt_tokens", inputTokens);
        usage.put("completion_tokens", outputTokens);
        usage.put("total_tokens", totalTokens);
        data.put("usage", usage);
        result.put("data", data);
        return ResponseEntity.ok(result);
    }

    private boolean checkCharacter(SseEmitter sseEmitter, User user, AskReq askReq) {
        try {

            //check 1: the character has been deleted
            Character delConv = characterService.lambdaQuery()
                    .eq(Character::getUuid, askReq.getCharacterUuid())
                    .eq(Character::getIsDeleted, true)
                    .one();
            if (null != delConv) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage("A_CHARACTER_DELETED"));
                return false;
            }

            //check 2: character quota
            Long convsCount = characterService.lambdaQuery()
                    .eq(Character::getUserId, user.getId())
                    .eq(Character::getIsDeleted, false)
                    .count();
            long convsMax = Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.CHARACTER_MAX_NUM));
            if (convsCount >= convsMax) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage("A_CHARACTER_MAX_LIMIT", String.valueOf(convsMax)));
                return false;
            }

            //check 3: current user's quota
            AiModel aiModel = LLMContext.getAiModel(askReq.getModelPlatform(), askReq.getModelName());
            if (null != aiModel && !aiModel.getIsFree()) {
                ErrorEnum errorMsg = quotaHelper.checkTextQuota(user);
                if (null != errorMsg) {
                    sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage(errorMsg.getInfo()));
                    return false;
                }
            }
        } catch (Exception e) {
            log.error("error", e);
            sseEmitter.completeWithError(e);
            return false;
        }
        return true;
    }

    @Async
    public void asyncCheckAndChat(SseEmitter sseEmitter, User user, AskReq askReq) {
        log.info("asyncCheckAndChat,userId:{}", user.getId());
        //check business rules
        if (!checkCharacter(sseEmitter, user, askReq)) {
            return;
        }
        //questions
        //system message
        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, askReq.getCharacterUuid())
                .oneOpt()
                .orElse(null);
        if (null == character) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage(A_CHARACTER_NOT_FOUND.getInfo()));
            return;
        }

        //Send analysing question event to client
        SSEEmitterHelper.sendPartial(sseEmitter, SSEEventName.STATE_CHANGED, SSEEventData.STATE_QUESTION_ANALYSING);
//If voice input, convert audio to text
        //如果是语音输入，将音频转成文本
        if (StringUtils.isNotBlank(askReq.getAudioUuid())) {
            String path = fileService.getImagePath(askReq.getAudioUuid());
            String audioText = new AsrModelContext().audioToText(path);
            if (StringUtils.isBlank(audioText)) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, SpringUtil.getMessage("B_ASR_PARSE_FAIL"));
                return;
            }
            askReq.setPrompt(audioText);
        }

        AbstractLLMService llmService = LLMContext.getServiceOrDefault(askReq.getModelPlatform(), askReq.getModelName());

//If knowledge bases are associated, filter valid ones for subsequent queries
        //如果关联了知识库，筛选出有效的知识库以待后续查询，同时发送搜索知识库事件给前端用户
        List<KbInfoResp> filteredKb = new ArrayList<>();
        if (StringUtils.isNotBlank(character.getKbIds())) {
            List<Long> kbIds = Arrays.stream(character.getKbIds().split(",")).map(Long::parseLong).toList();
            filteredKb = characterService.filterEnableKb(user, kbIds);

            //Send searching knowledge event to user
            SSEEmitterHelper.sendPartial(sseEmitter, SSEEventName.STATE_CHANGED, SSEEventData.STATE_KNOWLEDGE_SEARCHING);
        }
        //Retrieve contents from knowledge base and character memory
        List<RetrieverWrapper> retrieverWrappers = retrieve(character.getId(), filteredKb, llmService, askReq);

        // Process prompt with retrieved contents and audio settings
        int answerContentType = getAnswerContentType(character, askReq);
        boolean answerToAudio = TtsUtil.needTts(llmService.getTtsSetting(), answerContentType);
        String effectiveLocale = StringUtils.isNotBlank(user.getLocale())
                ? user.getLocale()
                : Objects.toString(SysConfigService.getByKey(AdiConstant.SysConfigKey.DEFAULT_LOCALE), "zh-CN");
        Pair<String, String> memoryAndKnowledge = buildMemoryAndKnowledge(retrieverWrappers);
        String audioExtra = answerToAudio ? (effectiveLocale.startsWith("zh") ? PROMPT_EXTRA_AUDIO : PROMPT_EXTRA_AUDIO_EN) : "";
        String processedPrompt = PromptUtil.createPrompt(askReq.getPrompt(), memoryAndKnowledge.getLeft(), memoryAndKnowledge.getRight(), audioExtra, effectiveLocale);
        if (!Objects.equals(askReq.getPrompt(), processedPrompt)) {
            askReq.setProcessedPrompt(processedPrompt);
        }

        String questionUuid = StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid()) ? askReq.getRegenerateQuestionUuid() : UuidUtil.createShort();
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUser(user);
        sseAskParams.setUuid(questionUuid);
        sseAskParams.setModelName(askReq.getModelName());
        sseAskParams.setSseEmitter(sseEmitter);
        sseAskParams.setRegenerateQuestionUuid(askReq.getRegenerateQuestionUuid());
        sseAskParams.setAnswerContentType(answerContentType);
        if (null != character.getAudioConfig() && null != character.getAudioConfig().getVoice()) {
//If character has voice configured, use character voice settings
            //如果对话配置了语音，则使用对话的语音配置
            sseAskParams.setVoice(character.getAudioConfig().getVoice().getParamName());
        }

        ChatModelRequestParams chatRequestParams = buildChatRequestParams(character, askReq);
        sseAskParams.setHttpRequestParams(chatRequestParams);

        sseAskParams.setModelProperties(
                ChatModelBuilderProperties.builder()
                        .temperature(character.getLlmTemperature())
                        .returnThinking(chatRequestParams.getReturnThinking())
                        .build()
        );
        sseEmitterHelper.call(sseAskParams, (response, questionMeta, answerMeta) -> {

            AudioInfo audioInfo = null;
            if (StringUtils.isNotBlank(response.getAudioPath())) {

                audioInfo = new AudioInfo();
                MultimediaInfo multimediaInfo = LocalFileUtil.getAudioFileInfo(response.getAudioPath());
                if (null != multimediaInfo) {
                    audioInfo.setDuration((int) multimediaInfo.getDuration() / 1000);
                }
                audioInfo.setPath(response.getAudioPath());
                //存储到数据库并返回真实的URL
                AdiFile adiFile = fileService.saveFromPath(user, response.getAudioPath());
                audioInfo.setUuid(adiFile.getUuid());
                audioInfo.setUrl(FileOperatorContext.getFileUrl(adiFile));
            }
            boolean isRefEmbedding = false;
            boolean isRefGraph = false;
            boolean isRefMemoryEmbedding = false;
            for (RetrieverWrapper wrapper : retrieverWrappers) {
                if (RetrieveContentFrom.KNOWLEDGE_BASE.equals(wrapper.getContentFrom())) {
                    if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever embeddingStoreContentRetriever) {
                        isRefEmbedding = !embeddingStoreContentRetriever.getRetrievedEmbeddingToScore().isEmpty();
                    } else if (wrapper.getRetriever() instanceof GraphStoreContentRetriever graphStoreContentRetriever) {
                        RefGraphDto graphDto = graphStoreContentRetriever.getGraphRef();
                        isRefGraph = !graphDto.getVertices().isEmpty() || !graphDto.getEdges().isEmpty();
                    }
                } else if (RetrieveContentFrom.CHARACTER_MEMORY.equals(wrapper.getContentFrom())) {
                    //目前记忆相关内容只使用向量存储，后续如果增加了其他类型的记忆存储，也可以在这里增加判断
                    if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever embeddingStoreContentRetriever) {
                        isRefMemoryEmbedding = !embeddingStoreContentRetriever.getRetrievedEmbeddingToScore().isEmpty();
                    }
                }
            }
            answerMeta.setIsRefEmbedding(isRefEmbedding);
            answerMeta.setIsRefGraph(isRefGraph);
            answerMeta.setIsRefMemoryEmbedding(isRefMemoryEmbedding);
            sseEmitterHelper.sendComplete(user.getId(), sseEmitter, questionMeta, answerMeta, audioInfo);
            self.saveAfterAiResponse(user, askReq, retrieverWrappers, response, questionMeta, answerMeta, audioInfo);
        });
    }

    /**
     * 判断是否需要返回推理过程
     * 以下两种场景表示需要返回推理过程
     * 场景1：模型是推理模型 && 不允许关闭推理过程，
     * 场景2：模型是推理模型 && 允许关闭推理过程 && 角色会话开启了深度思考
     *
     * @param aiModel      模型
     * @param character 会话
     * @return 是否需要返回推理过程
     */
    private Boolean checkIfReturnThinking(AiModel aiModel, Character character) {
        if (!aiModel.getIsReasoner()) {
            return null;
        }
        return Boolean.FALSE.equals(aiModel.getIsThinkingClosable()) || Boolean.TRUE.equals(character.getIsEnableThinking());
    }

    /**
     * 多知识库搜索、记忆搜索
     *
     * @param characterId     角色id
     * @param filteredKb 有效的已关联的知识库
     * @param llmService 大模型服务
     * @param askReq     请求参数
     */
    private List<RetrieverWrapper> retrieve(Long characterId, List<KbInfoResp> filteredKb, AbstractLLMService llmService, AskReq askReq) {
        ChatModel chatModel = llmService.buildChatLLM(
                ChatModelBuilderProperties.builder()
                        .temperature(LLM_TEMPERATURE_DEFAULT)
                        .build());
        //Create memory retriever
        RetrieverCreateParam memoryRetrieveParam = RetrieverCreateParam.builder()
                .chatModel(chatModel)
                .filter(new IsEqualTo(CHARACTER_ID, characterId))
                .maxResults(3)
                .minScore(RAG_RETRIEVE_MIN_SCORE_DEFAULT)
                .breakIfSearchMissed(false)
                .build();
        List<RetrieverWrapper> retrieverWrappers = new CompositeRag(RetrieveContentFrom.CHARACTER_MEMORY).createRetriever(memoryRetrieveParam);
        //Create knowledge base retriever
        if (!filteredKb.isEmpty()) {
            List<String> kbUuids = filteredKb.stream().map(KbInfoResp::getUuid).toList();
            log.info("Preparing to search related knowledge bases, kbUuids:{}, question:{}", String.join(",", kbUuids), askReq.getPrompt());
            //忽略知识库自身的设置如 [最大召回数量maxResult，最小命中分数minScore，角色设置，是否强行中断搜索] 等
            RetrieverCreateParam kbRetrieveParam = RetrieverCreateParam.builder()
                    .chatModel(chatModel)
                    .filter(new IsIn(KB_UUID, kbUuids)) //跨多个知识库查询
                    .maxResults(3)
                    .minScore(RAG_RETRIEVE_MIN_SCORE_DEFAULT)
                    .breakIfSearchMissed(false)
                    .build();
            List<RetrieverWrapper> kbRetrievers = new CompositeRag(RetrieveContentFrom.KNOWLEDGE_BASE).createRetriever(kbRetrieveParam);
            retrieverWrappers.addAll(kbRetrievers);
        }
        //Retrieve contents concurrently
        if (!retrieverWrappers.isEmpty()) {
            CountDownLatch countDownLatch = new CountDownLatch(retrieverWrappers.size());
            for (RetrieverWrapper retriever : retrieverWrappers) {
                mainExecutor.execute(() -> {
                    try {
                        List<Content> contents = retriever.getRetriever().retrieve(Query.from(askReq.getPrompt()));
                        retriever.setResponse(contents);
                    } catch (Exception e) {
                        log.error("Retrieve content error", e);
                    } finally {
                        countDownLatch.countDown();
                    }
                });
            }
            try {
                boolean awaitRet = countDownLatch.await(1, TimeUnit.MINUTES);
                if (!awaitRet) {
                    log.warn("retrieveContents CountDownLatch await timeout");
                }
            } catch (InterruptedException e) {
                log.error("retrieveContents CountDownLatch await error", e);
                Thread.currentThread().interrupt();
            }
        }
        return retrieverWrappers;
    }

    public List<CharacterMessage> listQuestionsByCharacterId(long characterId, long maxId, int pageSize) {
        LambdaQueryWrapper<CharacterMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(CharacterMessage::getCharacterId, characterId);
        queryWrapper.eq(CharacterMessage::getParentMessageId, 0);
        queryWrapper.lt(CharacterMessage::getId, maxId);
        queryWrapper.eq(CharacterMessage::getIsDeleted, false);
        queryWrapper.last("limit " + pageSize);
        queryWrapper.orderByDesc(CharacterMessage::getId);
        return getBaseMapper().selectList(queryWrapper);
    }

    @Transactional
    public void saveAfterAiResponse(User user, AskReq askReq, List<RetrieverWrapper> retrievers, LLMResponseContent response, PromptMeta questionMeta, AnswerMeta answerMeta, AudioInfo audioInfo) {
        Character character;
        String prompt = askReq.getPrompt();
        String characterUuid = askReq.getCharacterUuid();
        String modelPlatform = askReq.getModelPlatform();
        String modelName = askReq.getModelName();
        character = characterService.lambdaQuery()
                .eq(Character::getUuid, characterUuid)
                .eq(Character::getUserId, user.getId())
                .oneOpt()
                .orElseGet(() -> characterService.createByFirstMessage(user.getId(), characterUuid, prompt));
        AiModel aiModel = LLMContext.getAiModel(modelPlatform, modelName);

        //Check if regenerate question
        CharacterMessage promptMsg;
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            promptMsg = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid());
        } else {
            //Save new question message
            CharacterMessage question = new CharacterMessage();
            question.setUserId(user.getId());
            question.setUuid(questionMeta.getUuid());
            question.setCharacterId(character.getId());
            question.setCharacterUuid(characterUuid);
            question.setMessageRole(ChatMessageRoleEnum.USER.getValue());
            question.setRemark(prompt);
            question.setProcessedRemark(askReq.getProcessedPrompt());
            question.setAiModelId(aiModel.getId());
            question.setAudioUuid(askReq.getAudioUuid());
            question.setAudioDuration(askReq.getAudioDuration());
            question.setTokens(questionMeta.getTokens());
            question.setUnderstandContextMsgPairNum(user.getUnderstandContextMsgPairNum());
            question.setAttachments(String.join(",", askReq.getImageUrls()));
            baseMapper.insert(question);

            promptMsg = this.lambdaQuery().eq(CharacterMessage::getUuid, questionMeta.getUuid()).one();

        }

        //save response message
        CharacterMessage aiAnswer = new CharacterMessage();
        aiAnswer.setUserId(user.getId());
        aiAnswer.setUuid(answerMeta.getUuid());
        aiAnswer.setCharacterId(character.getId());
        aiAnswer.setCharacterUuid(characterUuid);
        aiAnswer.setMessageRole(ChatMessageRoleEnum.ASSISTANT.getValue());
        aiAnswer.setThinkingContent(Objects.toString(response.getThinkingContent(), ""));
        aiAnswer.setRemark(response.getContent());
//TODO: Filter or transform AI returned content
        //TODO 过滤或转换AI返回的内容
        //aiAnswer.setProcessedRemark("");
        aiAnswer.setAudioUuid(null == audioInfo ? "" : Objects.toString(audioInfo.getUuid(), ""));
        aiAnswer.setAudioDuration(null == audioInfo ? 0 : audioInfo.getDuration());
        aiAnswer.setTokens(answerMeta.getTokens());
        aiAnswer.setParentMessageId(promptMsg.getId());
        aiAnswer.setAiModelId(aiModel.getId());
        aiAnswer.setIsRefEmbedding(answerMeta.getIsRefEmbedding());
        aiAnswer.setIsRefGraph(answerMeta.getIsRefGraph());
        aiAnswer.setIsRefMemoryEmbedding(answerMeta.getIsRefMemoryEmbedding());
        int answerContentType = getAnswerContentType(character, askReq);
        aiAnswer.setContentType(answerContentType);
        baseMapper.insert(aiAnswer);

        createRef(retrievers, user, aiAnswer.getId());

        calcTodayCost(user, character, questionMeta, answerMeta, aiModel.getIsFree());

        //Short-term memory
        if (Boolean.TRUE.equals(character.getUnderstandContextEnable())) {
            MapDBChatMemoryStore mapDBChatMemoryStore = MapDBChatMemoryStore.getSingleton();
            List<ChatMessage> messages = mapDBChatMemoryStore.getMessages(askReq.getCharacterUuid());
            List<ChatMessage> newMessages = new ArrayList<>(messages);
// TODO: DeepSeek requires reasoning_content to be passed back to API
            // TODO: DeepSeek 要求 reasoning_content 传回 API，升级 langchain4j 后确认是否仍需手动处理
            newMessages.add(AiMessage.builder().text(response.getContent()).thinking(response.getThinkingContent()).build());
            mapDBChatMemoryStore.updateMessages(askReq.getCharacterUuid(), newMessages);
        }

// TODO: Some vision models like qwen2-vl-7b-instruct do not support JSON structured response
        // TODO... 部分视觉模型如 qwen2-vl-7b-instruct 不支持 json 结构返回内容，待处理
        if (!aiModel.getType().equalsIgnoreCase(ModelType.VISION)) {
            //Long-term memory
            longTermMemoryService.asyncAdd(character.getId(), modelPlatform, modelName, askReq.getPrompt(), response.getContent());
            //TODO async calculate token cost and update user day cost (include long-term memory analyze cost)
            // Pair<Integer, Integer> inputOutputTokenCost = LLMTokenUtil.calAllTokenCostByUuid(stringRedisTemplate, updateQaParams.getSseAskParams().getUuid());
        }
    }

    private void calcTodayCost(User user, Character character, PromptMeta questionMeta, AnswerMeta answerMeta, boolean isFreeToken) {

        int todayTokenCost = questionMeta.getTokens() + answerMeta.getTokens();
        try {
            //calculate character tokens
            characterService.lambdaUpdate()
                    .eq(Character::getId, character.getId())
                    .set(Character::getTokens, character.getTokens() + todayTokenCost)
                    .update();

            userDayCostService.appendCostToUser(user, todayTokenCost, isFreeToken);
        } catch (Exception e) {
            log.error("calcTodayCost error", e);
        }
    }

    private CharacterMessage getPromptMsgByQuestionUuid(String questionUuid) {
        return this.lambdaQuery().eq(CharacterMessage::getUuid, questionUuid).oneOpt().orElseThrow(() -> new BaseException(B_MESSAGE_NOT_FOUND));
    }

    public boolean softDelete(String uuid) {
        return this.lambdaUpdate()
                .eq(CharacterMessage::getUuid, uuid)
                .eq(CharacterMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(CharacterMessage::getIsDeleted, false)
                .set(CharacterMessage::getIsDeleted, true)
                .update();
    }

    public String getTextByAudioUuid(String audioUuid) {
        if (StringUtils.isBlank(audioUuid)) {
            return null;
        }
        CharacterMessage conversationMessage = this.lambdaQuery()
                .eq(CharacterMessage::getAudioUuid, audioUuid)
                .eq(!ThreadContext.getCurrentUser().getIsAdmin(), CharacterMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(CharacterMessage::getIsDeleted, false)
                .last("limit 1")
                .oneOpt()
                .orElse(null);
        if (null == conversationMessage) {
            return null;
        }
        return conversationMessage.getRemark();
    }

    private void createRef(List<RetrieverWrapper> wrappers, User user, Long msgId) {
        if (CollectionUtils.isEmpty(wrappers)) {
            return;
        }
        for (RetrieverWrapper wrapper : wrappers) {
            if (RetrieveContentFrom.KNOWLEDGE_BASE.equals(wrapper.getContentFrom())) {
                if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever embeddingRetriever) {
                    self.createEmbeddingRefs(user, msgId, embeddingRetriever.getRetrievedEmbeddingToScore());
                } else if (wrapper.getRetriever() instanceof GraphStoreContentRetriever graphRetriever) {
                    self.createGraphRefs(user, msgId, graphRetriever.getGraphRef());
                }
            } else if (RetrieveContentFrom.CHARACTER_MEMORY.equals(wrapper.getContentFrom())) {
                if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever knowledgeBaseRetriever) {
                    self.createMemoryRefs(user, msgId, knowledgeBaseRetriever.getRetrievedEmbeddingToScore());
                }
            }

        }
    }

    /**
     * 增加嵌入引用记录
     *
     * @param user             用户
     * @param messageId        消息id
     * @param embeddingToScore 嵌入向量id和分数的映射
     */
    public void createEmbeddingRefs(User user, Long messageId, Map<String, Double> embeddingToScore) {
        log.info("Creating vector reference, userId:{}, qaRecordId:{}, embeddingToScore.size:{}", user.getId(), messageId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            CharacterMessageRefEmbedding recordReference = new CharacterMessageRefEmbedding();
            recordReference.setMessageId(messageId);
            recordReference.setEmbeddingId(embeddingId);
            recordReference.setScore(embeddingToScore.get(embeddingId));
            recordReference.setUserId(user.getId());
            characterMessageRefEmbeddingService.save(recordReference);
        }
    }

    public void createMemoryRefs(User user, Long messageId, Map<String, Double> embeddingToScore) {
        log.info("Creating memory vector reference, userId:{}, qaRecordId:{}, embeddingToScore.size:{}", user.getId(), messageId, embeddingToScore.size());
        for (Map.Entry<String, Double> entry : embeddingToScore.entrySet()) {
            String embeddingId = entry.getKey();
            CharacterMessageRefMemoryEmbedding refEmb = new CharacterMessageRefMemoryEmbedding();
            refEmb.setMessageId(messageId);
            refEmb.setEmbeddingId(embeddingId);
            refEmb.setScore(embeddingToScore.get(embeddingId));
            refEmb.setUserId(user.getId());
            characterMessageRefMemoryEmbeddingService.save(refEmb);
        }
    }

    /**
     * 增加图谱引用记录
     *
     * @param user      用户
     * @param messageId 消息id
     * @param graphDto  图谱引用数据传输对象
     */
    public void createGraphRefs(User user, Long messageId, RefGraphDto graphDto) {
        log.info("Preparing to create graph reference, userId:{}, qaRecordId:{}, vertices.Size:{}, edges.size:{}", user.getId(), messageId, graphDto.getVertices().size(), graphDto.getEdges().size());
        if (graphDto.getVertices().isEmpty() && graphDto.getEdges().isEmpty()) {
            log.warn("Graph reference data is empty, cannot create graph reference record, userId:{}, qaRecordId:{}", user.getId(), messageId);
            return;
        }
        String entities = null == graphDto.getEntitiesFromQuestion() ? "" : String.join(",", graphDto.getEntitiesFromQuestion());
        Map<String, Object> graphFromStore = new HashMap<>();
        graphFromStore.put("vertices", graphDto.getVertices());
        graphFromStore.put("edges", graphDto.getEdges());
        CharacterMessageRefGraph refGraph = new CharacterMessageRefGraph();
        refGraph.setMessageId(messageId);
        refGraph.setUserId(user.getId());
        refGraph.setGraphFromLlm(entities);
        refGraph.setGraphFromStore(JsonUtil.toJson(graphFromStore));
        characterMessageRefGraphService.save(refGraph);
    }

    private ChatModelRequestParams buildChatRequestParams(Character character, AskReq askReq) {
        ChatModelRequestParams.ChatModelRequestParamsBuilder builder = ChatModelRequestParams.builder();
        if (StringUtils.isNotBlank(character.getAiSystemMessage())) {
            builder.systemMessage(character.getAiSystemMessage());
        }
        //history message
        if (Boolean.TRUE.equals(character.getUnderstandContextEnable())) {
            builder.memoryId(askReq.getCharacterUuid());
        }
//If user question has been processed (e.g., with retrieved document segments), use the enhanced question
        //如果用户问题已处理过，例如增加了召回的文档文段，则使用该增强的问题，否则使用用户的原始问题
        String prompt = StringUtils.isNotBlank(askReq.getProcessedPrompt()) ? askReq.getProcessedPrompt() : askReq.getPrompt();
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            CharacterMessage lastMsg = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid());
            prompt = StringUtils.isNotBlank(lastMsg.getProcessedRemark()) ? lastMsg.getProcessedRemark() : lastMsg.getRemark();
        }
        builder.userMessage(prompt);
        builder.imageUrls(askReq.getImageUrls());

        List<McpClient> mcpClients = new ArrayList<>();
        if (StringUtils.isNotBlank(character.getMcpIds())) {
            List<Long> mcpIds = stringToList(character.getMcpIds(), ",", Long::parseLong);
            mcpClients = userMcpService.createMcpClients(character.getUserId(), mcpIds);
        }
        builder.mcpClients(mcpClients);

        //Enable thinking
        AiModel aiModel = LLMContext.getServiceOrDefault(askReq.getModelPlatform(), askReq.getModelName()).getAiModel();
        Boolean returnThinking = checkIfReturnThinking(aiModel, character);
        builder.returnThinking(returnThinking);

        //Enable web search
        builder.enableWebSearch(Boolean.TRUE.equals(character.getIsEnableWebSearch()));

        return builder.build();
    }

    /**
     * 获取响应内容类型
     *
     * @param character 对话
     * @param askReq       请求参数
     * @return 响应内容类型
     */
    private int getAnswerContentType(Character character, AskReq askReq) {
        int answerContentType = character.getAnswerContentType();
//If response content type is auto and user input is audio, set response content type to audio
        //如果设置了响应内容类型为自动，并且用户输入是音频，则响应内容类型设置为音频
        if (answerContentType == AdiConstant.CharacterConstant.ANSWER_CONTENT_TYPE_AUTO && StringUtils.isNotBlank(askReq.getAudioUuid())) {
            answerContentType = AdiConstant.CharacterConstant.ANSWER_CONTENT_TYPE_AUDIO;
        }
        return answerContentType;
    }

    private Pair<String, String> buildMemoryAndKnowledge(List<RetrieverWrapper> wrappers) {
        StringBuilder memory = new StringBuilder();
        StringBuilder knowledge = new StringBuilder();
        wrappers.forEach(item -> {
            String retrieveType = item.getContentFrom();
            if (RetrieveContentFrom.CHARACTER_MEMORY.equals(retrieveType)) {
                for (Content content : item.getResponse()) {
                    memory.append(content.textSegment().text()).append("\n");
                }
                if (memory.isEmpty()) {
                    memory.append("None\n");
                } else {
                    memory.append("\n");
                }
            } else if (RetrieveContentFrom.KNOWLEDGE_BASE.equals(retrieveType)) {
                for (Content content : item.getResponse()) {
                    knowledge.append(content.textSegment().text()).append("\n");
                }
                if (knowledge.isEmpty()) {
                    knowledge.append("None\n");
                } else {
                    knowledge.append("\n");
                }
            }
        });
        return Pair.of(memory.toString(), knowledge.toString());
    }
}
