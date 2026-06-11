package com.moyz.adi.common.service;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.CharacterMessage;
import com.moyz.adi.common.entity.LLMCallRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ChatMessageRoleEnum;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.file.LocalFileUtil;
import com.moyz.adi.common.helper.AsrModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.SseManager;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.languagemodel.data.LLMResponseContent;
import com.moyz.adi.common.memory.longterm.LongTermMemoryService;
import com.moyz.adi.common.memory.shortterm.MapDBChatMemoryStore;
import com.moyz.adi.common.rag.AdiEmbeddingStoreContentRetriever;
import com.moyz.adi.common.rag.GraphStoreContentRetriever;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import ws.schild.jave.info.MultimediaInfo;

import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static com.moyz.adi.common.enums.ErrorEnum.A_CHARACTER_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;

/**
 * Chat orchestration service: orchestrates the full SSE/blocking chat pipeline.
 *
 * <p>Agent = Character in execution. This service coordinates:
 * AgentService (LLM pipeline), CharacterService (Character data), CharacterMessageService (message CRUD).</p>
 *
 * <p>Dependency direction: CharacterChatService → AgentService → CharacterService → CharacterMessageService</p>
 */
@Slf4j
@Service
public class CharacterChatService {

    @Lazy
    @Resource
    private CharacterChatService self;

    @Resource
    private CharacterService characterService;

    @Resource
    private CharacterMessageService characterMessageService;

    @Resource
    private AgentService agentService;

    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Resource
    private SseManager sseManager;

    @Resource
    private FileService fileService;

    @Resource
    private AsyncTaskExecutor mainExecutor;

    @Resource
    private LongTermMemoryService longTermMemoryService;

    @Resource
    private LLMCallRecordService llmCallRecordService;

    public SseEmitter sseAsk(AskReq askReq) {
        String sseUuid = UuidUtil.createShort();
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        User user = ThreadContext.getCurrentUser();
        if (!sseManager.checkOrComplete(user, sseUuid, sseEmitter)) {
            return sseEmitter;
        }
        sseManager.startSse(user, sseUuid, sseEmitter, null);
        self.asyncCheckAndChat(sseUuid, user, askReq);
        return sseEmitter;
    }

    /**
     * Blocking chat: calls AgentService for the core pipeline, then persists messages to DB.
     */
    public Map<String, Object> blockingAsk(AskReq askReq) {
        User user = ThreadContext.getExistCurrentUser();

        // Validate character exists
        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, askReq.getCharacterUuid())
                .eq(Character::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_CHARACTER_NOT_FOUND));

        // Quota check
        AiModel aiModel = LLMContext.getAiModel(askReq.getModelPlatform(), askReq.getModelName());
        if (null != aiModel && !aiModel.getIsFree()) {
            ErrorEnum quotaError = quotaHelper.checkTextQuota(user);
            if (null != quotaError) {
                throw new BaseException(quotaError);
            }
        }

        // Build generic request and invoke via AgentService
        String questionUuid = UuidUtil.createShort();
        AgentRequest request = AgentRequest.builder()
                .characterUuid(askReq.getCharacterUuid())
                .modelPlatform(askReq.getModelPlatform())
                .modelName(askReq.getModelName())
                .inputText(askReq.getPrompt())
                .enableRag(true)
                .enableMcp(true)
                .enableWebSearch(true)
                .build();
        AgentResult result = agentService.invoke(request, user, questionUuid);

        // Persist messages to DB
        PromptMeta questionMeta = new PromptMeta(
                result.getInputTokens() != null ? result.getInputTokens() : 0, questionUuid);
        AnswerMeta answerMeta = AnswerMeta.builder()
                .tokens(result.getOutputTokens() != null ? result.getOutputTokens() : 0)
                .uuid(UuidUtil.createShort())
                .build();
        LLMResponseContent responseContent = new LLMResponseContent(
                result.getThinking(), result.getAnswer(), null);
        self.saveAfterAiResponse(user, askReq, new ArrayList<>(), responseContent, questionMeta, answerMeta, null);

        // Build response
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("message_id", questionUuid);
        data.put("answer", result.getAnswer());
        Map<String, Object> usage = new LinkedHashMap<>();
        usage.put("prompt_tokens", result.getInputTokens() != null ? result.getInputTokens() : 0);
        usage.put("completion_tokens", result.getOutputTokens() != null ? result.getOutputTokens() : 0);
        usage.put("total_tokens", (result.getInputTokens() != null ? result.getInputTokens() : 0)
                + (result.getOutputTokens() != null ? result.getOutputTokens() : 0));
        data.put("usage", usage);
        return data;
    }

    private boolean checkCharacter(String sseUuid, User user, AskReq askReq) {
        try {

            //check 1: the character has been deleted
            Character delConv = characterService.lambdaQuery()
                    .eq(Character::getUuid, askReq.getCharacterUuid())
                    .eq(Character::getIsDeleted, true)
                    .one();
            if (null != delConv) {
                sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage("A_CHARACTER_DELETED"));
                return false;
            }

            //check 2: character quota
            Long convsCount = characterService.lambdaQuery()
                    .eq(Character::getUserId, user.getId())
                    .eq(Character::getIsDeleted, false)
                    .count();
            long convsMax = Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.CHARACTER_MAX_NUM));
            if (convsCount >= convsMax) {
                sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage("A_CHARACTER_MAX_LIMIT", String.valueOf(convsMax)));
                return false;
            }

            //check 3: current user's quota
            AiModel aiModel = LLMContext.getAiModel(askReq.getModelPlatform(), askReq.getModelName());
            if (null != aiModel && !aiModel.getIsFree()) {
                ErrorEnum errorMsg = quotaHelper.checkTextQuota(user);
                if (null != errorMsg) {
                    sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage(errorMsg.getInfo()));
                    return false;
                }
            }
        } catch (Exception e) {
            log.error("error", e);
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, e.getMessage());
            return false;
        }
        return true;
    }

    @Async
    public void asyncCheckAndChat(String sseUuid, User user, AskReq askReq) {
        log.info("asyncCheckAndChat,userId:{}", user.getId());
        //check business rules
        if (!checkCharacter(sseUuid, user, askReq)) {
            return;
        }
        //questions
        //system message
        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, askReq.getCharacterUuid())
                .oneOpt()
                .orElse(null);
        if (null == character) {
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage(A_CHARACTER_NOT_FOUND.getInfo()));
            return;
        }

        //Send analysing question event to client
        SseManager.sendPartial(sseUuid, SSEEventName.STATE_CHANGED, SSEEventData.STATE_QUESTION_ANALYSING);
//If voice input, convert audio to text
        //如果是语音输入，将音频转成文本
        if (StringUtils.isNotBlank(askReq.getAudioUuid())) {
            String path = fileService.getImagePath(askReq.getAudioUuid());
            String audioText = new AsrModelContext().audioToText(path);
            if (StringUtils.isBlank(audioText)) {
                sseManager.sendErrorAndComplete(user.getId(), sseUuid, SpringUtil.getMessage("B_ASR_PARSE_FAIL"));
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
            SseManager.sendPartial(sseUuid, SSEEventName.STATE_CHANGED, SSEEventData.STATE_KNOWLEDGE_SEARCHING);
        }
        //Retrieve contents from knowledge base and character memory
        List<RetrieverWrapper> retrieverWrappers = CharacterChatHelper.retrieve(character.getId(), filteredKb, llmService, askReq.getPrompt(), mainExecutor);

        // Process prompt with retrieved contents and audio settings
        int answerContentType = getAnswerContentType(character, askReq);
        boolean answerToAudio = TtsUtil.needTts(llmService.getTtsSetting(), answerContentType);
        String effectiveLocale = StringUtils.isNotBlank(user.getLocale())
                ? user.getLocale()
                : Objects.toString(SysConfigService.getByKey(AdiConstant.SysConfigKey.DEFAULT_LOCALE), "zh-CN");
        Pair<String, String> memoryAndKnowledge = CharacterChatHelper.buildMemoryAndKnowledge(retrieverWrappers);
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
        sseAskParams.setSseUuid(sseUuid);
        sseAskParams.setRegenerateQuestionUuid(askReq.getRegenerateQuestionUuid());
        sseAskParams.setAnswerContentType(answerContentType);
        if (null != character.getAudioConfig() && null != character.getAudioConfig().getVoice()) {
//If character has voice configured, use character voice settings
            //如果对话配置了语音，则使用对话的语音配置
            sseAskParams.setVoice(character.getAudioConfig().getVoice().getParamName());
        }

        ChatModelRequestParams chatRequestParams = CharacterChatHelper.buildChatRequestParams(character, askReq.getProcessedPrompt() != null ? askReq.getProcessedPrompt() : askReq.getPrompt(), user, llmService, true, Boolean.TRUE.equals(character.getIsEnableWebSearch()), askReq.getImageUrls());
        sseAskParams.setHttpRequestParams(chatRequestParams);

        sseAskParams.setModelProperties(
                ChatModelBuilderProperties.builder()
                        .temperature(character.getLlmTemperature())
                        .returnThinking(chatRequestParams.getReturnThinking())
                        .build()
        );
        try {
            sseManager.call(sseAskParams, (response, questionMeta, answerMeta) -> {

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
                sseManager.sendComplete(user.getId(), sseUuid, questionMeta, answerMeta, audioInfo);
                self.saveAfterAiResponse(user, askReq, retrieverWrappers, response, questionMeta, answerMeta, audioInfo);
            });
        } catch (Exception e) {
            log.error("Chat pipeline failed, userId:{}, sseUuid:{}", user.getId(), sseUuid, e);
            sseManager.sendErrorAndComplete(user.getId(), sseUuid, e.getMessage());
        }
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
            question.setUnderstandContextMsgPairNum(user.getUnderstandContextMsgPairNum());
            question.setAttachments(String.join(",", askReq.getImageUrls()));
            characterMessageService.save(question);

            promptMsg = characterMessageService.lambdaQuery().eq(CharacterMessage::getUuid, questionMeta.getUuid()).one();

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
        aiAnswer.setParentMessageId(promptMsg.getId());
        aiAnswer.setAiModelId(aiModel.getId());
        aiAnswer.setIsRefEmbedding(answerMeta.getIsRefEmbedding());
        aiAnswer.setIsRefGraph(answerMeta.getIsRefGraph());
        aiAnswer.setIsRefMemoryEmbedding(answerMeta.getIsRefMemoryEmbedding());
        int answerContentType = getAnswerContentType(character, askReq);
        aiAnswer.setContentType(answerContentType);
        characterMessageService.save(aiAnswer);

        //Save LLM call record
        LLMCallRecord callRecord = new LLMCallRecord();
        callRecord.setUuid(UuidUtil.createShort());
        callRecord.setSourceType(LLMCallRecordSourceType.CHARACTER_CHAT.getValue());
        callRecord.setSourceId(aiAnswer.getId());
        callRecord.setUserId(user.getId());
        callRecord.setModelPlatform(modelPlatform);
        callRecord.setModelName(modelName);
        callRecord.setInputTokens(questionMeta.getTokens());
        callRecord.setOutputTokens(answerMeta.getTokens());
        llmCallRecordService.saveAsync(callRecord);

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
        return characterMessageService.lambdaQuery().eq(CharacterMessage::getUuid, questionUuid).oneOpt().orElseThrow(() -> new BaseException(B_MESSAGE_NOT_FOUND));
    }

    private void createRef(List<RetrieverWrapper> wrappers, User user, Long msgId) {
        if (CollectionUtils.isEmpty(wrappers)) {
            return;
        }
        for (RetrieverWrapper wrapper : wrappers) {
            if (RetrieveContentFrom.KNOWLEDGE_BASE.equals(wrapper.getContentFrom())) {
                if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever embeddingRetriever) {
                    characterMessageService.createEmbeddingRefs(user, msgId, embeddingRetriever.getRetrievedEmbeddingToScore());
                } else if (wrapper.getRetriever() instanceof GraphStoreContentRetriever graphRetriever) {
                    characterMessageService.createGraphRefs(user, msgId, graphRetriever.getGraphRef());
                }
            } else if (RetrieveContentFrom.CHARACTER_MEMORY.equals(wrapper.getContentFrom())) {
                if (wrapper.getRetriever() instanceof AdiEmbeddingStoreContentRetriever knowledgeBaseRetriever) {
                    characterMessageService.createMemoryRefs(user, msgId, knowledgeBaseRetriever.getRetrievedEmbeddingToScore());
                }
            }

        }
    }

    /**
     * 获取响应内容类型
     *
     * @param character 对话
     * @param askReq    请求参数
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
}
