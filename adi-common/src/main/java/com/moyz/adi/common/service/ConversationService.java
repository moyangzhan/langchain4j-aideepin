package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.ConversationMapper;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.*;
import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class ConversationService extends ServiceImpl<ConversationMapper, Conversation> {

    @Lazy
    @Resource
    private ConversationService self;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private ConversationMessageService conversationMessageService;

    @Resource
    private ConversationPresetService conversationPresetService;

    @Resource
    private ConversationPresetRelService conversationPresetRelService;

    @Resource
    private UserMcpService userMcpService;

    @Resource
    private FileService fileService;

    public Page<ConvDto> search(ConvSearchReq convSearchReq, int currentPage, int pageSize) {
        Page<Conversation> page = this.lambdaQuery()
                .eq(Conversation::getIsDeleted, false)
                .like(!StringUtils.isBlank(convSearchReq.getTitle()), Conversation::getTitle, convSearchReq.getTitle())
                .orderByDesc(Conversation::getId)
                .page(new Page<>(currentPage, pageSize));
        return MPPageUtil.convertToPage(page, ConvDto.class);
    }

    public List<ConvDto> listByUser() {
        List<Conversation> list = this.lambdaQuery()
                .eq(Conversation::getUserId, ThreadContext.getCurrentUserId())
                .eq(Conversation::getIsDeleted, false)
                .orderByDesc(Conversation::getId)
                .last("limit " + sysConfigService.getConversationMaxNum())
                .list();
        return MPPageUtil.convertToList(list, ConvDto.class, (source, target) -> {
            if (StringUtils.isNotBlank(source.getMcpIds())) {
                target.setMcpIds(Arrays.stream(source.getMcpIds().split(","))
                        .map(Long::parseLong)
                        .toList());
            } else {
                target.setMcpIds(Collections.emptyList());
            }
            return target;
        });
    }

    /**
     * 查询对话{@code uuid}的消息列表
     *
     * @param uuid       对话的uuid
     * @param maxMsgUuid 最大uuid（转换成id进行判断）
     * @param pageSize   每页数量
     * @return 列表
     */
    public ConvMsgListResp detail(String uuid, String maxMsgUuid, int pageSize) {
        Conversation conversation = this.lambdaQuery().eq(Conversation::getUuid, uuid).one();
        if (null == conversation) {
            throw new BaseException(A_CONVERSATION_NOT_EXIST);
        }

        long maxId = Long.MAX_VALUE;
        if (StringUtils.isNotBlank(maxMsgUuid)) {
            ConversationMessage maxMsg = conversationMessageService.lambdaQuery()
                    .select(ConversationMessage::getId)
                    .eq(ConversationMessage::getUuid, maxMsgUuid)
                    .eq(ConversationMessage::getIsDeleted, false)
                    .one();
            if (null == maxMsg) {
                throw new BaseException(A_DATA_NOT_FOUND);
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
        List<ConvMsgDto> userMessages = MPPageUtil.convertToList(questions, ConvMsgDto.class, (source, target) -> {
            if (StringUtils.isNotBlank(source.getAttachments())) {
                List<String> urls = fileService.getUrls(Arrays.stream(source.getAttachments().split(",")).toList());
                target.setAttachmentUrls(urls);
            } else {
                target.setAttachmentUrls(Collections.emptyList());
            }
            return target;
        });
        ConvMsgListResp result = new ConvMsgListResp(minUuid, userMessages);

        //Wrap answer content
        List<Long> parentIds = questions.stream().map(ConversationMessage::getId).toList();
        List<ConversationMessage> childMessages = conversationMessageService
                .lambdaQuery()
                .in(ConversationMessage::getParentMessageId, parentIds)
                .eq(ConversationMessage::getIsDeleted, false)
                .list();
        Map<Long, List<ConversationMessage>> idToMessages = childMessages.stream().collect(Collectors.groupingBy(ConversationMessage::getParentMessageId));

        //Fill AI answer to the request of user
        result.getMsgList().forEach(item -> {
            List<ConvMsgDto> children = MPPageUtil.convertToList(idToMessages.get(item.getId()), ConvMsgDto.class);
            if (children.size() > 1) {
                children = children.stream().sorted(Comparator.comparing(ConvMsgDto::getCreateTime).reversed()).toList();
            }

            for (ConvMsgDto convMsgDto : children) {
                AiModel aiModel = MODEL_ID_TO_OBJ.get(convMsgDto.getAiModelId());
                convMsgDto.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            }
            item.setChildren(children);
        });
        return result;
    }

    public int createDefault(Long userId) {
        Conversation conversation = new Conversation();
        conversation.setUuid(UuidUtil.createShort());
        conversation.setUserId(userId);
        conversation.setTitle(AdiConstant.ConversationConstant.DEFAULT_NAME);
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

    public ConvDto add(ConvAddReq convAddReq) {
        Conversation conversation = this.lambdaQuery()
                .eq(Conversation::getUserId, ThreadContext.getCurrentUserId())
                .eq(Conversation::getTitle, convAddReq.getTitle())
                .eq(Conversation::getIsDeleted, false)
                .one();
        if (null != conversation) {
            throw new BaseException(A_CONVERSATION_TITLE_EXIST);
        }

        List<Long> filteredMcpIds = findEnableMcpIds(convAddReq.getMcpIds());

        String uuid = UuidUtil.createShort();
        Conversation one = new Conversation();
        one.setUuid(uuid);
        one.setTitle(convAddReq.getTitle());
        one.setAiSystemMessage(convAddReq.getAiSystemMessage());
        one.setUserId(ThreadContext.getCurrentUserId());
        one.setRemark(convAddReq.getRemark());
        one.setMcpIds(StringUtils.join(filteredMcpIds, ","));
        baseMapper.insert(one);

        Conversation conv = this.lambdaQuery().eq(Conversation::getUuid, uuid).one();
        return MPPageUtil.convertTo(conv, ConvDto.class);
    }

    /**
     * 根据预设会话创建当前用户会话
     *
     * @param presetConvUuid 预设会话uuid
     */
    public ConvDto addByPresetConv(String presetConvUuid) {
        ConversationPreset presetConv = this.conversationPresetService.lambdaQuery()
                .eq(ConversationPreset::getUuid, presetConvUuid)
                .eq(ConversationPreset::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_PRESET_CONVERSATION_NOT_EXIST));
        ConversationPresetRel presetRel = this.conversationPresetRelService.lambdaQuery()
                .eq(ConversationPresetRel::getUserId, ThreadContext.getCurrentUserId())
                .eq(ConversationPresetRel::getPresetConvId, presetConv.getId())
                .eq(ConversationPresetRel::getIsDeleted, false)
                .oneOpt()
                .orElse(null);
        if (null != presetRel) {
            Conversation conv = this.getById(presetRel.getUserConvId());
            return MPPageUtil.convertTo(conv, ConvDto.class);
        }
        ConvAddReq convAddReq = ConvAddReq.builder()
                .title(presetConv.getTitle())
                .remark(presetConv.getRemark())
                .aiSystemMessage(presetConv.getAiSystemMessage())
                .build();
        ConvDto convDto = self.add(convAddReq);
        conversationPresetRelService.save(
                ConversationPresetRel.builder()
                        .presetConvId(presetConv.getId())
                        .userConvId(convDto.getId())
                        .userId(ThreadContext.getCurrentUserId())
                        .build()
        );
        return convDto;
    }

    public boolean edit(String uuid, ConvEditReq convEditReq) {
        Conversation conversation = getOrThrow(uuid);
        Conversation one = new Conversation();
        BeanUtils.copyProperties(convEditReq, one);
        one.setId(conversation.getId());
        if (null != convEditReq.getUnderstandContextEnable()) {
            one.setUnderstandContextEnable(convEditReq.getUnderstandContextEnable());
        }
        if (null != convEditReq.getMcpIds()) {
            List<Long> filteredMcpIds = findEnableMcpIds(convEditReq.getMcpIds());
            one.setMcpIds(StringUtils.join(filteredMcpIds, ","));
        }
        return baseMapper.updateById(one) > 0;
    }

    @Transactional
    public boolean softDel(String uuid) {
        Conversation conversation = getOrThrow(uuid);
        conversationPresetRelService.softDelBy(conversation.getUserId(), conversation.getId());
        return this.lambdaUpdate()
                .eq(Conversation::getId, conversation.getId())
                .set(Conversation::getIsDeleted, true)
                .update();
    }

    public int countTodayCreated() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime beginTime = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0, 0);
        LocalDateTime endTime = beginTime.plusDays(1);
        return baseMapper.countCreatedByTimePeriod(beginTime, endTime);
    }

    public int countAllCreated() {
        return baseMapper.countAllCreated();
    }

    private Conversation getOrThrow(String uuid) {
        Conversation conversation = this.lambdaQuery()
                .eq(Conversation::getUuid, uuid)
                .eq(Conversation::getIsDeleted, false)
                .one();
        if (null == conversation) {
            throw new BaseException(A_CONVERSATION_NOT_EXIST);
        }
        if (!conversation.getUserId().equals(ThreadContext.getCurrentUserId()) && !ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
        return conversation;
    }

    /**
     * 过滤出有效的MCP服务id列表 | Filter the list of valid MCP service IDs
     *
     * @param mcpIdsInReq 请求中传入的MCP服务id列表 | List of MCP service IDs passed in the request
     * @return 有效的MCP服务id列表 | List of valid MCP service IDs
     */
    private List<Long> findEnableMcpIds(List<Long> mcpIdsInReq) {
        List<Long> result = new ArrayList<>();
        if (CollectionUtils.isEmpty(mcpIdsInReq)) {
            return result;
        }
        List<UserMcp> userMcpList = userMcpService.searchEnableByUserId(ThreadContext.getCurrentUserId());

        for (Long mcpIdInReq : mcpIdsInReq) {
            if (userMcpList.stream().anyMatch(item -> item.getMcpId().equals(mcpIdInReq))) {
                result.add(mcpIdInReq);
            } else {
                log.warn("User mcp id {} not found or disabled in user mcp list, userId: {}, mcpId:{}", mcpIdInReq, ThreadContext.getCurrentUserId(), mcpIdInReq);
            }
        }
        return result;
    }
}
