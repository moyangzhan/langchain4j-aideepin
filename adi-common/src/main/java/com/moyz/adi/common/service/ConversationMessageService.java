package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.entity.Conversation;
import com.moyz.adi.common.entity.ConversationMessage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ChatMessageRoleEnum;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.mapper.ConversationMessageMapper;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.PromptMeta;
import com.moyz.adi.common.vo.SseAskParams;
import com.theokanning.openai.completion.chat.ChatMessageRole;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.memory.ChatMemory;
import dev.langchain4j.memory.chat.TokenWindowChatMemory;
import dev.langchain4j.model.openai.OpenAiTokenizer;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.Comparator;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;
import static dev.langchain4j.model.openai.OpenAiModelName.GPT_3_5_TURBO;

@Slf4j
@Service
public class ConversationMessageService extends ServiceImpl<ConversationMessageMapper, ConversationMessage> {

    @Lazy
    @Resource
    private ConversationMessageService _this;

    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Lazy
    @Resource
    private ConversationService conversationService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;


    public SseEmitter sseAsk(AskReq askReq) {
        SseEmitter sseEmitter = new SseEmitter();
        User user = ThreadContext.getCurrentUser();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        sseEmitterHelper.startSse(user, sseEmitter);
        _this.asyncCheckAndPushToClient(sseEmitter, ThreadContext.getCurrentUser(), askReq);
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
            ErrorEnum errorMsg = quotaHelper.checkTextQuota(user);
            if (null != errorMsg) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, errorMsg.getInfo());
                return false;
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

        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setModelName(askReq.getModelName());
        String prompt = askReq.getPrompt();
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            prompt = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid()).getRemark();
        }
        sseAskParams.setSystemMessage(StringUtils.EMPTY);
        sseAskParams.setSseEmitter(sseEmitter);
        sseAskParams.setUserMessage(prompt);
        sseAskParams.setRegenerateQuestionUuid(askReq.getRegenerateQuestionUuid());
        //questions
        //system message
        Conversation conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, askReq.getConversationUuid())
                .oneOpt()
                .orElse(null);
        if (null != conversation) {
            if (StringUtils.isNotBlank(conversation.getAiSystemMessage())) {
                sseAskParams.setSystemMessage(conversation.getAiSystemMessage());
            }
            //history message
            if (Boolean.TRUE.equals(conversation.getUnderstandContextEnable())) {
                sseAskParams.setMessageId(askReq.getConversationUuid());
            }
//                List<ConversationMessage> historyMsgList = this.lambdaQuery()
//                        .eq(ConversationMessage::getUserId, user.getId())
//                        .eq(ConversationMessage::getConversationUuid, askReq.getConversationUuid())
//                        .orderByDesc(ConversationMessage::getId)
//                        .last("limit " + user.getUnderstandContextMsgPairNum() * 2)
//                        .list();
//                if (!historyMsgList.isEmpty()) {
//                    ChatMemory chatMemory = TokenWindowChatMemory.withMaxTokens(1000, new OpenAiTokenizer(GPT_3_5_TURBO));
//                    historyMsgList.sort(Comparator.comparing(ConversationMessage::getId));
//                    for (ConversationMessage historyMsg : historyMsgList) {
//                        if (ChatMessageRoleEnum.USER.getValue().equals(historyMsg.getMessageRole())) {
//                            UserMessage userMessage = UserMessage.from(historyMsg.getRemark());
//                            chatMemory.add(userMessage);
//                        } else if (ChatMessageRoleEnum.SYSTEM.getValue().equals(historyMsg.getMessageRole())) {
//                            SystemMessage systemMessage = SystemMessage.from(historyMsg.getRemark());
//                            chatMemory.add(systemMessage);
//                        }else if (ChatMessageRoleEnum.ASSISTANT.getValue().equals(historyMsg.getMessageRole())) {
//                            AiMessage aiMessage = AiMessage.from(historyMsg.getRemark());
//                            chatMemory.add(aiMessage);
//                        }
//                    }
//                    sseAskParams.setChatMemory(chatMemory);
//                }
//            }
        }
        sseEmitterHelper.processAndPushToModel(user, sseAskParams, (response, questionMeta, answerMeta) -> {
            _this.saveAfterAiResponse(user, askReq, response, questionMeta, answerMeta);
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
    public void saveAfterAiResponse(User user, AskReq askReq, String response, PromptMeta questionMeta, AnswerMeta answerMeta) {

        int secretKeyType = StringUtils.isNotBlank(user.getSecretKey()) ? AdiConstant.SECRET_KEY_TYPE_CUSTOM : AdiConstant.SECRET_KEY_TYPE_SYSTEM;
        Conversation conversation;
        String prompt = askReq.getPrompt();
        String convUuid = askReq.getConversationUuid();
        conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, convUuid)
                .eq(Conversation::getUserId, user.getId())
                .oneOpt()
                .orElseGet(() -> conversationService.createByFirstMessage(user.getId(), convUuid, prompt));
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
            question.setTokens(questionMeta.getTokens());
            question.setSecretKeyType(secretKeyType);
            question.setUnderstandContextMsgPairNum(user.getUnderstandContextMsgPairNum());
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
        aiAnswer.setRemark(response);
        aiAnswer.setTokens(answerMeta.getTokens());
        aiAnswer.setParentMessageId(promptMsg.getId());
        aiAnswer.setSecretKeyType(secretKeyType);
        aiAnswer.setLanguageModelName(askReq.getModelName());
        baseMapper.insert(aiAnswer);

        calcTodayCost(user, conversation, questionMeta, answerMeta);

    }

    private void calcTodayCost(User user, Conversation conversation, PromptMeta questionMeta, AnswerMeta answerMeta) {

        int todayTokenCost = questionMeta.getTokens() + answerMeta.getTokens();
        try {
            //calculate conversation tokens
            conversationService.lambdaUpdate()
                    .eq(Conversation::getId, conversation.getId())
                    .set(Conversation::getTokens, conversation.getTokens() + todayTokenCost)
                    .update();

            userDayCostService.appendCostToUser(user, todayTokenCost);
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

}
