package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.ChatMessageRoleEnum;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.file.LocalFileUtil;
import com.moyz.adi.common.helper.AsrModelContext;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.mapper.ConversationMessageMapper;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.MapDBChatMemoryStore;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.mcp.client.McpClient;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import ws.schild.jave.info.MultimediaInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.moyz.adi.common.cosntant.AdiConstant.SSE_TIMEOUT;
import static com.moyz.adi.common.enums.ErrorEnum.A_CONVERSATION_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;
import static com.moyz.adi.common.util.AdiStringUtil.stringToList;

@Slf4j
@Service
public class ConversationMessageService extends ServiceImpl<ConversationMessageMapper, ConversationMessage> {

    @Lazy
    @Resource
    private ConversationMessageService self;

    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Lazy
    @Resource
    private ConversationService conversationService;

    @Resource
    private UserMcpService userMcpService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;

    @Resource
    private FileService fileService;


    public SseEmitter sseAsk(AskReq askReq) {
        SseEmitter sseEmitter = new SseEmitter(SSE_TIMEOUT);
        User user = ThreadContext.getCurrentUser();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        sseEmitterHelper.startSse(user, sseEmitter);
        self.asyncCheckAndPushToClient(sseEmitter, user, askReq);
        return sseEmitter;
    }

    private boolean checkConversation(SseEmitter sseEmitter, User user, AskReq askReq) {
        try {

            //check 1: the conversation has been deleted
            Conversation delConv = conversationService.lambdaQuery()
                    .eq(Conversation::getUuid, askReq.getConversationUuid())
                    .eq(Conversation::getIsDeleted, true)
                    .one();
            if (null != delConv) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, "该对话已经删除");
                return false;
            }

            //check 2: conversation quota
            Long convsCount = conversationService.lambdaQuery()
                    .eq(Conversation::getUserId, user.getId())
                    .eq(Conversation::getIsDeleted, false)
                    .count();
            long convsMax = Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.CONVERSATION_MAX_NUM));
            if (convsCount >= convsMax) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, "对话数量已经达到上限，当前对话上限为：" + convsMax);
                return false;
            }

            //check 3: current user's quota
            AiModel aiModel = LLMContext.getAiModel(askReq.getModelName());
            if (null != aiModel && !aiModel.getIsFree()) {
                ErrorEnum errorMsg = quotaHelper.checkTextQuota(user);
                if (null != errorMsg) {
                    sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, errorMsg.getInfo());
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
    public void asyncCheckAndPushToClient(SseEmitter sseEmitter, User user, AskReq askReq) {
        log.info("asyncCheckAndPushToClient,userId:{}", user.getId());
        //check business rules
        if (!checkConversation(sseEmitter, user, askReq)) {
            return;
        }
        //questions
        //system message
        Conversation conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, askReq.getConversationUuid())
                .oneOpt()
                .orElse(null);
        if (null == conversation) {
            sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, A_CONVERSATION_NOT_FOUND.getInfo());
            return;
        }

        //如果是语音输入，将音频转成文本
        if (StringUtils.isNotBlank(askReq.getAudioUuid())) {
            String path = fileService.getImagePath(askReq.getAudioUuid());
            String audioText = new AsrModelContext().audioToText(path);
            if (StringUtils.isBlank(audioText)) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, "音频解析失败，请检查音频文件是否正确");
                return;
            }
            askReq.setPrompt(audioText);
        }

        String questionUuid = StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid()) ? askReq.getRegenerateQuestionUuid() : UuidUtil.createShort();
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUser(user);
        sseAskParams.setUuid(questionUuid);
        sseAskParams.setModelName(askReq.getModelName());
        sseAskParams.setSseEmitter(sseEmitter);
        sseAskParams.setRegenerateQuestionUuid(askReq.getRegenerateQuestionUuid());

        int answerContentType = getAnswerContentType(conversation, askReq);
        sseAskParams.setAnswerContentType(answerContentType);

        ChatModelParams chatModelParams = buildChatModelParams(conversation, askReq);
        sseAskParams.setChatModelParams(chatModelParams);

        AiModel aiModel = LLMContext.getLLMServiceByName(askReq.getModelName()).getAiModel();
        boolean returnThinking = false;
        //模型是推理模型，并且不允许关闭推理过程
        if (Boolean.TRUE.equals(aiModel.getIsReasoner()) && Boolean.FALSE.equals(aiModel.getIsThinkingClosable())) {
            returnThinking = true;
        }
        //模型是推理模型，并且允许关闭推理过程，并且角色会话开启了推理过程
        else if (Boolean.TRUE.equals(aiModel.getIsReasoner()) && Boolean.TRUE.equals(aiModel.getIsThinkingClosable()) && Boolean.TRUE.equals(conversation.getIsEnableThinking())) {
            returnThinking = true;
        }
        sseAskParams.setLlmBuilderProperties(
                LLMBuilderProperties.builder()
                        .temperature(conversation.getLlmTemperature())
                        .returnThinking(returnThinking)
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
            sseEmitterHelper.sendComplete(user.getId(), sseEmitter, questionMeta, answerMeta, audioInfo);
            self.saveAfterAiResponse(user, askReq, response, questionMeta, answerMeta, audioInfo);
        });
    }

    public List<ConversationMessage> listQuestionsByConvId(long convId, long maxId, int pageSize) {
        LambdaQueryWrapper<ConversationMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ConversationMessage::getConversationId, convId);
        queryWrapper.eq(ConversationMessage::getParentMessageId, 0);
        queryWrapper.lt(ConversationMessage::getId, maxId);
        queryWrapper.eq(ConversationMessage::getIsDeleted, false);
        queryWrapper.last("limit " + pageSize);
        queryWrapper.orderByDesc(ConversationMessage::getId);
        return getBaseMapper().selectList(queryWrapper);
    }

    @Transactional
    public void saveAfterAiResponse(User user, AskReq askReq, LLMResponseContent response, PromptMeta questionMeta, AnswerMeta answerMeta, AudioInfo audioInfo) {
        Conversation conversation;
        String prompt = askReq.getPrompt();
        String convUuid = askReq.getConversationUuid();
        conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, convUuid)
                .eq(Conversation::getUserId, user.getId())
                .oneOpt()
                .orElseGet(() -> conversationService.createByFirstMessage(user.getId(), convUuid, prompt));
        AiModel aiModel = LLMContext.getAiModel(askReq.getModelName());
        //Check if regenerate question
        ConversationMessage promptMsg;
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            promptMsg = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid());
        } else {
            //Save new question message
            ConversationMessage question = new ConversationMessage();
            question.setUserId(user.getId());
            question.setUuid(questionMeta.getUuid());
            question.setConversationId(conversation.getId());
            question.setConversationUuid(convUuid);
            question.setMessageRole(ChatMessageRoleEnum.USER.getValue());
            question.setRemark(prompt);
            question.setAiModelId(aiModel.getId());
            question.setAudioUuid(askReq.getAudioUuid());
            question.setAudioDuration(askReq.getAudioDuration());
            question.setTokens(questionMeta.getTokens());
            question.setUnderstandContextMsgPairNum(user.getUnderstandContextMsgPairNum());
            question.setAttachments(String.join(",", askReq.getImageUrls()));
            baseMapper.insert(question);

            promptMsg = this.lambdaQuery().eq(ConversationMessage::getUuid, questionMeta.getUuid()).one();
        }

        //save response message
        ConversationMessage aiAnswer = new ConversationMessage();
        aiAnswer.setUserId(user.getId());
        aiAnswer.setUuid(answerMeta.getUuid());
        aiAnswer.setConversationId(conversation.getId());
        aiAnswer.setConversationUuid(convUuid);
        aiAnswer.setMessageRole(ChatMessageRoleEnum.ASSISTANT.getValue());
        aiAnswer.setThinkingContent(Objects.toString(response.getThinkingContent(), ""));
        aiAnswer.setRemark(response.getContent());
        aiAnswer.setAudioUuid(null == audioInfo ? "" : Objects.toString(audioInfo.getUuid(), ""));
        aiAnswer.setAudioDuration(null == audioInfo ? 0 : audioInfo.getDuration());
        aiAnswer.setTokens(answerMeta.getTokens());
        aiAnswer.setParentMessageId(promptMsg.getId());
        aiAnswer.setAiModelId(aiModel.getId());
        int answerContentType = getAnswerContentType(conversation, askReq);
        aiAnswer.setContentType(answerContentType);
        baseMapper.insert(aiAnswer);

        calcTodayCost(user, conversation, questionMeta, answerMeta, aiModel.getIsFree());

        //Save response to memory
        if (Boolean.TRUE.equals(conversation.getUnderstandContextEnable())) {
            MapDBChatMemoryStore mapDBChatMemoryStore = MapDBChatMemoryStore.getSingleton();
            List<ChatMessage> messages = mapDBChatMemoryStore.getMessages(askReq.getConversationUuid());
            List<ChatMessage> newMessages = new ArrayList<>(messages);
            newMessages.add(AiMessage.aiMessage(response.getContent()));
            mapDBChatMemoryStore.updateMessages(askReq.getConversationUuid(), newMessages);
        }
    }

    private void calcTodayCost(User user, Conversation conversation, PromptMeta questionMeta, AnswerMeta answerMeta, boolean isFreeToken) {

        int todayTokenCost = questionMeta.getTokens() + answerMeta.getTokens();
        try {
            //calculate conversation tokens
            conversationService.lambdaUpdate()
                    .eq(Conversation::getId, conversation.getId())
                    .set(Conversation::getTokens, conversation.getTokens() + todayTokenCost)
                    .update();

            userDayCostService.appendCostToUser(user, todayTokenCost, isFreeToken);
        } catch (Exception e) {
            log.error("calcTodayCost error", e);
        }
    }

    private ConversationMessage getPromptMsgByQuestionUuid(String questionUuid) {
        return this.lambdaQuery().eq(ConversationMessage::getUuid, questionUuid).oneOpt().orElseThrow(() -> new BaseException(B_MESSAGE_NOT_FOUND));
    }

    public boolean softDelete(String uuid) {
        return this.lambdaUpdate()
                .eq(ConversationMessage::getUuid, uuid)
                .eq(ConversationMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(ConversationMessage::getIsDeleted, false)
                .set(ConversationMessage::getIsDeleted, true)
                .update();
    }

    public String getTextByAudioUuid(String audioUuid) {
        if (StringUtils.isBlank(audioUuid)) {
            return null;
        }
        ConversationMessage conversationMessage = this.lambdaQuery()
                .eq(ConversationMessage::getAudioUuid, audioUuid)
                .eq(!ThreadContext.getCurrentUser().getIsAdmin(), ConversationMessage::getUserId, ThreadContext.getCurrentUserId())
                .eq(ConversationMessage::getIsDeleted, false)
                .last("limit 1")
                .oneOpt()
                .orElse(null);
        if (null == conversationMessage) {
            return null;
        }
        return conversationMessage.getRemark();
    }

    private ChatModelParams buildChatModelParams(Conversation conversation, AskReq askReq) {
        ChatModelParams.ChatModelParamsBuilder builder = ChatModelParams.builder();
        if (StringUtils.isNotBlank(conversation.getAiSystemMessage())) {
            builder.systemMessage(conversation.getAiSystemMessage());
        }
        //history message
        if (Boolean.TRUE.equals(conversation.getUnderstandContextEnable())) {
            builder.memoryId(askReq.getConversationUuid());
        }
        String prompt = askReq.getPrompt();
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            prompt = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid()).getRemark();
        }
        builder.userMessage(prompt);
        builder.imageUrls(askReq.getImageUrls());

        List<McpClient> mcpClients = new ArrayList<>();
        if (StringUtils.isNotBlank(conversation.getMcpIds())) {
            List<Long> mcpIds = stringToList(conversation.getMcpIds(), ",", Long::parseLong);
            mcpClients = userMcpService.createMcpClients(conversation.getUserId(), mcpIds);
        }
        builder.mcpClients(mcpClients);
        return builder.build();
    }

    /**
     * 获取响应内容类型
     *
     * @param conversation 对话
     * @param askReq       请求参数
     * @return 响应内容类型
     */
    private int getAnswerContentType(Conversation conversation, AskReq askReq) {
        int answerContentType = conversation.getAnswerContentType();
        //如果设置了响应内容类型为自动，并且用户输入是音频，则响应内容类型设置为音频
        if (answerContentType == AdiConstant.ConversationConstant.ANSWER_CONTENT_TYPE_AUTO && StringUtils.isNotBlank(askReq.getAudioUuid())) {
            answerContentType = AdiConstant.ConversationConstant.ANSWER_CONTENT_TYPE_AUDIO;
        }
        return answerContentType;
    }

}
