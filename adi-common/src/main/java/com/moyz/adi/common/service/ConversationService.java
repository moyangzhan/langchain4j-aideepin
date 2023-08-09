package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ConvDto;
import com.moyz.adi.common.dto.ConvEditReq;
import com.moyz.adi.common.dto.ConvMsgListResp;
import com.moyz.adi.common.dto.ConvMsgResp;
import com.moyz.adi.common.entity.Conversation;
import com.moyz.adi.common.entity.ConversationMessage;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.ConversationMapper;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.A_CONVERSATION_NOT_EXIST;

@Slf4j
@Service
public class ConversationService extends ServiceImpl<ConversationMapper, Conversation> {

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private ConversationMessageService conversationMessageService;

    public List<ConvDto> listByUser() {
        List<Conversation> list = this.lambdaQuery()
                .eq(Conversation::getUserId, ThreadContext.getCurrentUserId())
                .eq(Conversation::getIsDelete, false)
                .orderByDesc(Conversation::getId)
                .last("limit " + sysConfigService.getConversationMaxNum())
                .list();
        return MPPageUtil.convertTo(list, ConvDto.class);
    }

    /**
     * 查询对话{@code uuid}的消息列表
     *
     * @param uuid       对话的uuid
     * @param maxMsgUuid
     * @param pageSize
     * @return
     */
    public ConvMsgListResp detail(String uuid, String maxMsgUuid, int pageSize) {
        Conversation conversation = this.lambdaQuery().eq(Conversation::getUuid, uuid).one();
        if (null == conversation) {
            throw new RuntimeException("找不到对应的会话");
        }

        long maxId = Long.MAX_VALUE;
        if (StringUtils.isNotBlank(maxMsgUuid)) {
            ConversationMessage maxMsg = conversationMessageService.lambdaQuery()
                    .select(ConversationMessage::getId)
                    .eq(ConversationMessage::getUuid, maxMsgUuid)
                    .eq(ConversationMessage::getIsDelete, false)
                    .one();
            if (null == maxMsg) {
                throw new RuntimeException("找不到对应的消息");
            }
            maxId = maxMsg.getId();
        }

        List<ConversationMessage> questions = conversationMessageService.listQuestionsByConvId(conversation.getId(), maxId, pageSize);
        if (questions.isEmpty()) {
            return new ConvMsgListResp(StringUtils.EMPTY, Collections.emptyList());
        }
        String minUuid = questions.stream().reduce(questions.get(0), (a, b) -> {
            if (a.getId() < b.getId()) {
                return a;
            }
            return b;
        }).getUuid();
        //Wrap question content
        List<ConvMsgResp> userMessages = MPPageUtil.convertTo(questions, ConvMsgResp.class);
        ConvMsgListResp result = new ConvMsgListResp(minUuid, userMessages);

        //Wrap answer content
        List<Long> parentIds = questions.stream().map(ConversationMessage::getId).toList();
        List<ConversationMessage> childMessages = conversationMessageService
                .lambdaQuery()
                .in(ConversationMessage::getParentMessageId, parentIds)
                .eq(ConversationMessage::getIsDelete, false)
                .list();
        Map<Long, List<ConversationMessage>> idToMessages = childMessages.stream().collect(Collectors.groupingBy(ConversationMessage::getParentMessageId));

        //Fill AI answer to the request of user
        result.getMsgList().forEach(item -> {
            List<ConvMsgResp> children = MPPageUtil.convertTo(idToMessages.get(item.getId()), ConvMsgResp.class);
            if (children.size() > 1) {
                children = children.stream().sorted(Comparator.comparing(ConvMsgResp::getCreateTime).reversed()).collect(Collectors.toList());
            }
            item.setChildren(children);
        });
        return result;
    }

    public int createDefault(Long userId) {
        String uuid = UUID.randomUUID().toString().replace("-", "");
        Conversation conversation = new Conversation();
        conversation.setUuid(uuid);
        conversation.setUserId(userId);
        conversation.setTitle("New Chat");
        return baseMapper.insert(conversation);
    }

    public Conversation createByFirstMessage(Long userId, String uuid, String title) {
        Conversation conversation = new Conversation();
        conversation.setUuid(uuid);
        conversation.setUserId(userId);
        conversation.setTitle(StringUtils.substring(title, 0, 45));
        baseMapper.insert(conversation);

        return this.lambdaQuery().eq(Conversation::getUuid, uuid).oneOpt().orElse(null);
    }

    public boolean edit(String uuid, ConvEditReq convEditReq) {
        Conversation conversation = this.lambdaQuery()
                .eq(Conversation::getUuid, uuid)
                .eq(Conversation::getUserId, ThreadContext.getCurrentUserId())
                .eq(Conversation::getIsDelete, false)
                .one();
        if (null == conversation) {
            throw new BaseException(A_CONVERSATION_NOT_EXIST);
        }
        Conversation one = new Conversation();
        BeanUtils.copyProperties(convEditReq, one);
        one.setId(conversation.getId());
        if (null != convEditReq.getUnderstandContextEnable()) {
            one.setUnderstandContextEnable(convEditReq.getUnderstandContextEnable());
        }
        return baseMapper.updateById(one) > 0 ? true : false;
    }

    public boolean softDel(String uuid) {
        return this.lambdaUpdate()
                .eq(Conversation::getUuid, uuid)
                .eq(Conversation::getUserId, ThreadContext.getCurrentUserId())
                .set(Conversation::getIsDelete, true)
                .update();
    }

}
