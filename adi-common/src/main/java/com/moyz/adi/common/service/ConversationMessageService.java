package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.entity.Conversation;
import com.moyz.adi.common.entity.ConversationMessage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.UserDayCost;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.OpenAiHelper;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.RateLimitHelper;
import com.moyz.adi.common.mapper.ConversationMessageMapper;
import com.moyz.adi.common.model.AnswerMeta;
import com.moyz.adi.common.model.QuestionMeta;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UserUtil;
import com.theokanning.openai.completion.chat.ChatMessage;
import com.theokanning.openai.completion.chat.ChatMessageRole;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.moyz.adi.common.enums.ErrorEnum.B_MESSAGE_NOT_FOUND;

@Slf4j
@Service
public class ConversationMessageService extends ServiceImpl<ConversationMessageMapper, ConversationMessage> {

    @Lazy
    @Resource
    private ConversationMessageService _this;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private OpenAiHelper openAiHelper;

    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Lazy
    @Resource
    private ConversationService conversationService;

    @Resource
    private RateLimitHelper rateLimitHelper;


    public SseEmitter sseAsk(AskReq askReq) {
        SseEmitter sseEmitter = new SseEmitter();
        User user = ThreadContext.getCurrentUser();
        _this.asyncCheckAndPushToClient(sseEmitter, user, askReq);
        return sseEmitter;
    }

    private boolean check(SseEmitter sseEmitter, User user, AskReq askReq) {
        try {
            String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
            String askingVal = stringRedisTemplate.opsForValue().get(askingKey);
            //check 1: still waiting response
            if (StringUtils.isNotBlank(askingVal)) {
                sendErrorMsg(sseEmitter, "正在回复中...");
                return false;
            }

            //check 2: the conversation has been deleted
            Conversation delConv = conversationService.lambdaQuery()
                    .eq(Conversation::getUuid, askReq.getConversationUuid())
                    .eq(Conversation::getIsDelete, true)
                    .one();
            if (null != delConv) {
                sendErrorMsg(sseEmitter, "该对话已经删除");
                return false;
            }

            //check 3: conversation quota
            Long convsCount = conversationService.lambdaQuery()
                    .eq(Conversation::getUserId, user.getId())
                    .eq(Conversation::getIsDelete, false)
                    .count();
            long convsMax = Integer.parseInt(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.CONVERSATION_MAX_NUM));
            if (convsCount >= convsMax) {
                sendErrorMsg(sseEmitter, "对话数量已经达到上限，当前对话上限为：" + convsMax);
                return false;
            }

            //check 4: current user's quota
            ErrorEnum errorMsg = quotaHelper.checkTextQuota(user);
            if (null != errorMsg) {
                sendErrorMsg(sseEmitter, errorMsg.getInfo());
                return false;
            }
        } catch (Exception e) {
            log.error("error", e);
            sseEmitter.completeWithError(e);
            return false;
        }
        return true;
    }

    private void sendErrorMsg(SseEmitter sseEmitter, String errorMsg) {
        try {
            sseEmitter.send(SseEmitter.event().name("error").data(errorMsg));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        sseEmitter.complete();
    }

    @Async
    public void asyncCheckAndPushToClient(SseEmitter sseEmitter, User user, AskReq askReq) {
        log.info("asyncCheckAndPushToClient,userId:{}", user.getId());
        //rate limit by system
        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        if (!rateLimitHelper.checkRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG)) {
            sendErrorMsg(sseEmitter, "访问太过频繁");
            return;
        }

        //check business rules
        if (!check(sseEmitter, user, askReq)) {
            return;
        }

        String askingKey = MessageFormat.format(RedisKeyConstant.USER_ASKING, user.getId());
        stringRedisTemplate.opsForValue().set(askingKey, "1", 15, TimeUnit.SECONDS);
        try {
            sseEmitter.send(SseEmitter.event().name("start"));
        } catch (IOException e) {
            log.error("error", e);
            sseEmitter.completeWithError(e);
            stringRedisTemplate.delete(askingKey);
            return;
        }

        rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG);

        sseEmitter.onCompletion(() -> {
            log.info("response complete,uid:{}", user.getId());
        });
        sseEmitter.onTimeout(() -> log.warn("sseEmitter timeout,uid:{},on timeout:{}", user.getId(), sseEmitter.getTimeout()));
        sseEmitter.onError(
                throwable -> {
                    try {
                        log.error("sseEmitter error,uid:{},on error:{}", user.getId(), throwable);
                        sseEmitter.send(SseEmitter.event().name("error").data(throwable.getMessage()));
                    } catch (IOException e) {
                        log.error("error", e);
                    } finally {
                        stringRedisTemplate.delete(askingKey);
                    }
                }
        );
        String prompt = askReq.getPrompt();
        if (StringUtils.isNotBlank(askReq.getRegenerateQuestionUuid())) {
            prompt = getPromptMsgByQuestionUuid(askReq.getRegenerateQuestionUuid()).getContent();
        }
        //questions
        final List<ChatMessage> chatMessageList = new ArrayList<>();
        //system message
        Conversation conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, askReq.getConversationUuid())
                .oneOpt()
                .orElse(null);
        if (null != conversation) {
            if (StringUtils.isNotBlank(conversation.getAiSystemMessage())) {
                ChatMessage chatMessage = new ChatMessage(ChatMessageRole.SYSTEM.value(), conversation.getAiSystemMessage());
                chatMessageList.add(chatMessage);
            }
            //history message
            if (Boolean.TRUE.equals(conversation.getUnderstandContextEnable()) && user.getUnderstandContextMsgPairNum() > 0) {
                List<ConversationMessage> historyMsgList = this.lambdaQuery()
                        .eq(ConversationMessage::getUserId, user.getId())
                        .eq(ConversationMessage::getConversationId, askReq.getConversationUuid())
                        .orderByDesc(ConversationMessage::getConversationId)
                        .last("limit " + user.getUnderstandContextMsgPairNum() * 2)
                        .list();
                if (!historyMsgList.isEmpty()) {
                    historyMsgList.sort(Comparator.comparing(ConversationMessage::getId));
                    for (ConversationMessage historyMsg : historyMsgList) {
                        ChatMessage chatMessage = new ChatMessage(historyMsg.getMessageRole(), historyMsg.getContent());
                        chatMessageList.add(chatMessage);
                    }
                }

            }
        }
        //new user message
        ChatMessage userMessage = new ChatMessage(ChatMessageRole.USER.value(), prompt);
        chatMessageList.add(userMessage);
        openAiHelper.sseAsk(user, askReq.getRegenerateQuestionUuid(), chatMessageList, sseEmitter, (response, questionMeta, answerMeta) -> {
            try {
                _this.saveAfterAiResponse(user, askReq, response, questionMeta, answerMeta);
            } catch (Exception e) {
                log.error("error:", e);
            } finally {
                stringRedisTemplate.delete(askingKey);
            }
        });
    }

    public List<ConversationMessage> listQuestionsByConvId(long convId, long maxId, int pageSize) {
        LambdaQueryWrapper<ConversationMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ConversationMessage::getConversationId, convId);
        queryWrapper.eq(ConversationMessage::getParentMessageId, 0);
        queryWrapper.lt(ConversationMessage::getId, maxId);
        queryWrapper.eq(ConversationMessage::getIsDelete, false);
        queryWrapper.last("limit " + pageSize);
        queryWrapper.orderByDesc(ConversationMessage::getId);
        return getBaseMapper().selectList(queryWrapper);
    }

    @Transactional
    public void saveAfterAiResponse(User user, AskReq askReq, String response, QuestionMeta questionMeta, AnswerMeta answerMeta) {

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
            question.setMessageRole(ChatMessageRole.USER.value());
            question.setContent(prompt);
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
        aiAnswer.setMessageRole(ChatMessageRole.ASSISTANT.value());
        aiAnswer.setContent(response);
        aiAnswer.setTokens(answerMeta.getTokens());
        aiAnswer.setParentMessageId(promptMsg.getId());
        aiAnswer.setSecretKeyType(secretKeyType);
        baseMapper.insert(aiAnswer);

        calcTodayCost(user, conversation, questionMeta, answerMeta);

    }

    private void calcTodayCost(User user, Conversation conversation, QuestionMeta questionMeta, AnswerMeta answerMeta) {

        int todayTokenCost = questionMeta.getTokens() + answerMeta.getTokens();
        try {
            //calculate conversation tokens
            conversationService.lambdaUpdate()
                    .eq(Conversation::getId, conversation.getId())
                    .set(Conversation::getTokens, conversation.getTokens() + todayTokenCost)
                    .update();

            UserDayCost userDayCost = userDayCostService.getTodayCost(user);
            UserDayCost saveOrUpdateInst = new UserDayCost();
            if (null == userDayCost) {
                saveOrUpdateInst.setUserId(user.getId());
                saveOrUpdateInst.setDay(LocalDateTimeUtil.getToday());
                saveOrUpdateInst.setTokens(todayTokenCost);
                saveOrUpdateInst.setRequests(1);
                saveOrUpdateInst.setSecretKeyType(UserUtil.getSecretType(user));
            } else {
                saveOrUpdateInst.setId(userDayCost.getId());
                saveOrUpdateInst.setTokens(userDayCost.getTokens() + todayTokenCost);
                saveOrUpdateInst.setRequests(userDayCost.getRequests() + 1);
            }
            userDayCostService.saveOrUpdate(saveOrUpdateInst);
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
                .eq(ConversationMessage::getIsDelete, false)
                .set(ConversationMessage::getIsDelete, true)
                .update();
    }

}
