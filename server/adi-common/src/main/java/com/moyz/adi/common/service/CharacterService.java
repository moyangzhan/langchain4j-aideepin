package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.CharacterMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.util.UuidUtil;

import java.util.Objects;
import com.moyz.adi.common.vo.AudioConfig;
import com.moyz.adi.common.vo.TtsSetting;
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
public class CharacterService extends ServiceImpl<CharacterMapper, Character> {

    @Lazy
    @Resource
    private CharacterService self;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private CharacterMessageService characterMessageService;

    @Resource
    private CharacterPresetService characterPresetService;

    @Resource
    private CharacterPresetRelService characterPresetRelService;

    @Resource
    private UserMcpService userMcpService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private FileService fileService;

    @Resource
    private AiModelService aiModelService;

    public Page<CharacterDto> search(CharacterSearchReq characterSearchReq, int currentPage, int pageSize) {
        Page<Character> page = this.lambdaQuery()
                .eq(Character::getIsDeleted, false)
                .like(!StringUtils.isBlank(characterSearchReq.getTitle()), Character::getTitle, characterSearchReq.getTitle())
                .orderByDesc(Character::getId)
                .page(new Page<>(currentPage, pageSize));
        return MPPageUtil.convertToPage(page, CharacterDto.class);
    }

    public List<CharacterDto> listByUser() {
        User user = ThreadContext.getCurrentUser();
        List<Character> list = this.lambdaQuery()
                .eq(Character::getUserId, user.getId())
                .eq(Character::getIsDeleted, false)
                .orderByDesc(Character::getId)
                .last("limit " + sysConfigService.getCharacterMaxNum())
                .list();
        return MPPageUtil.convertToList(list, CharacterDto.class, (source, target) -> {
            setMcpToDto(source, target);
            setKbInfoToDto(source, target);
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
    public CharacterMsgListResp detail(String uuid, String maxMsgUuid, int pageSize) {
        Character character = this.lambdaQuery().eq(Character::getUuid, uuid).one();
        if (null == character) {
            log.error("character not exist, uuid: {}", uuid);
            throw new BaseException(A_CHARACTER_NOT_EXIST);
        }

        long maxId = Long.MAX_VALUE;
        if (StringUtils.isNotBlank(maxMsgUuid)) {
            CharacterMessage maxMsg = characterMessageService.lambdaQuery()
                    .select(CharacterMessage::getId)
                    .eq(CharacterMessage::getUuid, maxMsgUuid)
                    .eq(CharacterMessage::getIsDeleted, false)
                    .one();
            if (null == maxMsg) {
                throw new BaseException(A_DATA_NOT_FOUND);
            }
            maxId = maxMsg.getId();
        }

        List<CharacterMessage> questions = characterMessageService.listQuestionsByCharacterId(character.getId(), maxId, pageSize);
        if (questions.isEmpty()) {
            return new CharacterMsgListResp(StringUtils.EMPTY, Collections.emptyList());
        }
        String minUuid = questions.stream().reduce(questions.get(0), (a, b) -> {
            if (a.getId() < b.getId()) {
                return a;
            }
            return b;
        }).getUuid();
        //Wrap question content
        List<CharacterMsgDto> userMessages = MPPageUtil.convertToList(questions, CharacterMsgDto.class, (source, target) -> {
            if (StringUtils.isNotBlank(source.getAttachments())) {
                List<String> urls = fileService.getUrls(Arrays.stream(source.getAttachments().split(",")).toList());
                target.setAttachmentUrls(urls);
            } else {
                target.setAttachmentUrls(Collections.emptyList());
            }
            if (StringUtils.isNotBlank(source.getAudioUuid())) {
                target.setAudioUrl(fileService.getUrl(source.getAudioUuid()));
            } else {
                target.setAudioUrl("");
            }
            return target;
        });
        CharacterMsgListResp result = new CharacterMsgListResp(minUuid, userMessages);

        //Wrap answer content
        List<Long> parentIds = questions.stream().map(CharacterMessage::getId).toList();
        List<CharacterMessage> childMessages = characterMessageService
                .lambdaQuery()
                .in(CharacterMessage::getParentMessageId, parentIds)
                .eq(CharacterMessage::getIsDeleted, false)
                .list();
        Map<Long, List<CharacterMessage>> idToMessages = childMessages.stream().collect(Collectors.groupingBy(CharacterMessage::getParentMessageId));

        //Fill AI answer to the request of user
        result.getMsgList().forEach(item -> {
            List<CharacterMsgDto> children = MPPageUtil.convertToList(idToMessages.get(item.getId()), CharacterMsgDto.class);
            if (children.size() > 1) {
                children = children.stream().sorted(Comparator.comparing(CharacterMsgDto::getCreateTime).reversed()).toList();
            }

            for (CharacterMsgDto characterMsgDto : children) {
                AiModel aiModel = MODEL_ID_TO_OBJ.get(characterMsgDto.getAiModelId());
                characterMsgDto.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
                if (StringUtils.isNotBlank(characterMsgDto.getAudioUuid())) {
                    characterMsgDto.setAudioUrl(fileService.getUrl(characterMsgDto.getAudioUuid()));
                } else {
                    characterMsgDto.setAudioUrl("");
                }
            }
            item.setChildren(children);
        });
        return result;
    }

    public int createDefault(Long userId) {
        return createDefault(userId, null);
    }

    public int createDefault(Long userId, String locale) {
        Character character = new Character();
        character.setUuid(UuidUtil.createShort());
        character.setUserId(userId);
        String defaultLocale = StringUtils.isNotBlank(locale) ? locale
                : Objects.toString(SysConfigService.getByKey(AdiConstant.SysConfigKey.DEFAULT_LOCALE), "zh-CN");
        character.setTitle(defaultLocale.startsWith("zh")
                ? AdiConstant.CharacterConstant.DEFAULT_NAME
                : AdiConstant.CharacterConstant.DEFAULT_NAME_EN);
        return baseMapper.insert(character);
    }

    public Character createByFirstMessage(Long userId, String uuid, String title) {
        Character character = new Character();
        character.setUuid(uuid);
        character.setUserId(userId);
        character.setTitle(StringUtils.substring(title, 0, 45));
        baseMapper.insert(character);

        return this.lambdaQuery().eq(Character::getUuid, uuid).oneOpt().orElse(null);
    }

    public CharacterDto add(CharacterAddReq characterAddReq) {
        Character character = this.lambdaQuery()
                .eq(Character::getUserId, ThreadContext.getCurrentUserId())
                .eq(Character::getTitle, characterAddReq.getTitle())
                .eq(Character::getIsDeleted, false)
                .one();
        if (null != character) {
            throw new BaseException(A_CHARACTER_TITLE_EXIST);
        }

        List<Long> filteredMcpIds = filterEnableMcpIds(characterAddReq.getMcpIds());
        List<Long> filteredKbIds = filterEnableKbIds(ThreadContext.getCurrentUser(), characterAddReq.getKbIds());

        String uuid = UuidUtil.createShort();
        Character one = new Character();
        BeanUtils.copyProperties(characterAddReq, one);
        one.setUuid(uuid);
        one.setUserId(ThreadContext.getCurrentUserId());
        one.setMcpIds(StringUtils.join(filteredMcpIds, ","));
        one.setKbIds(StringUtils.join(filteredKbIds, ","));
        if (null != characterAddReq.getAudioConfig()) {
            one.setAudioConfig(characterAddReq.getAudioConfig());
        }
        baseMapper.insert(one);

        Character saved = this.lambdaQuery().eq(Character::getUuid, uuid).one();
        CharacterDto dto = MPPageUtil.convertTo(saved, CharacterDto.class);
        setMcpToDto(saved, dto);
        setKbInfoToDto(saved, dto);
        return dto;
    }

    /**
     * 组装MCP信息
     *
     * @param character 对话信息
     * @param dto          对话DTO
     */
    private void setMcpToDto(Character character, CharacterDto dto) {
        if (StringUtils.isNotBlank(character.getMcpIds())) {
            dto.setMcpIds(Arrays.stream(character.getMcpIds().split(","))
                    .map(Long::parseLong)
                    .toList());
        } else {
            dto.setMcpIds(new ArrayList<>());
        }
    }

    /**
     * 组装已关联的知识库信息
     *
     * @param conv 对话信息
     * @param dto  对话DTO
     */
    private void setKbInfoToDto(Character character, CharacterDto dto) {
        //组装已关联的知识库信息
        List<Long> kids = new ArrayList<>();
        List<CharacterKnowledge> characterKnowledgeList = new ArrayList<>();
        if (StringUtils.isNotBlank(character.getKbIds())) {
            List<Long> kbIds = Arrays.stream(character.getKbIds().split(","))
                    .map(Long::parseLong)
                    .toList();
            knowledgeBaseService.listByIds(kbIds).forEach(kb -> {
                CharacterKnowledge characterKnowledge = convertToCharacterKbDto(ThreadContext.getCurrentUser(), kb);
                // Skip if not mine and not public
                if (!characterKnowledge.getIsMine() && !characterKnowledge.getIsPublic()) {
                    characterKnowledge.setKbInfo(null);
                    characterKnowledge.setIsEnable(false);
                }
                characterKnowledgeList.add(characterKnowledge);
                kids.add(kb.getId());
            });
        }
        dto.setKbIds(kids);
        dto.setCharacterKnowledgeList(characterKnowledgeList);
    }

    /**
     * 根据预设会话创建当前用户会话
     *
     * @param presetConvUuid 预设会话uuid
     */
    @Transactional
    public CharacterDto addByPresetCharacter(String presetConvUuid) {
        CharacterPreset presetCharacter = this.characterPresetService.lambdaQuery()
                .eq(CharacterPreset::getUuid, presetConvUuid)
                .eq(CharacterPreset::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_PRESET_CHARACTER_NOT_EXIST));
        CharacterPresetRel presetRel = this.characterPresetRelService.lambdaQuery()
                .eq(CharacterPresetRel::getUserId, ThreadContext.getCurrentUserId())
                .eq(CharacterPresetRel::getPresetCharacterId, presetCharacter.getId())
                .eq(CharacterPresetRel::getIsDeleted, false)
                .oneOpt()
                .orElse(null);
        if (null != presetRel) {
            Character character = this.getById(presetRel.getUserCharacterId());
            return MPPageUtil.convertTo(character, CharacterDto.class);
        }

        List<Long> kbIds = Collections.emptyList();
        if (StringUtils.isNotBlank(presetCharacter.getKbTitle())) {
            KbEditReq kbEditReq = new KbEditReq();
            kbEditReq.setTitle(presetCharacter.getKbTitle());
            kbEditReq.setRemark(presetCharacter.getTitle() + " - " + presetCharacter.getKbTitle());
            KnowledgeBase kb = knowledgeBaseService.saveOrUpdate(kbEditReq);
            kbIds = List.of(kb.getId());
        }

        CharacterAddReq characterAddReq = CharacterAddReq.builder()
                .title(presetCharacter.getTitle())
                .remark(presetCharacter.getRemark())
                .aiSystemMessage(presetCharacter.getAiSystemMessage())
                .kbIds(kbIds)
                .build();
        CharacterDto characterDto = self.add(characterAddReq);
        characterPresetRelService.save(
                CharacterPresetRel.builder()
                        .presetCharacterId(presetCharacter.getId())
                        .userCharacterId(characterDto.getId())
                        .userId(ThreadContext.getCurrentUserId())
                        .build()
        );
        return characterDto;
    }

    public boolean edit(String uuid, CharacterEditReq characterEditReq) {
        Character character = getOrThrow(uuid);
        Character one = new Character();
        BeanUtils.copyProperties(characterEditReq, one);
        one.setId(character.getId());
        if (null != characterEditReq.getUnderstandContextEnable()) {
            one.setUnderstandContextEnable(characterEditReq.getUnderstandContextEnable());
        }
        if (null != characterEditReq.getMcpIds()) {
            List<Long> filteredMcpIds = filterEnableMcpIds(characterEditReq.getMcpIds());
            if (filteredMcpIds.isEmpty()) {
                one.setMcpIds(StringUtils.join(filteredMcpIds, ","));
            }
        }
        if (null != characterEditReq.getKbIds()) {
            if (characterEditReq.getKbIds().isEmpty()) {
                one.setKbIds("");
            } else {
                List<Long> filteredKbIds = filterEnableKbIds(ThreadContext.getCurrentUser(), characterEditReq.getKbIds());
                one.setKbIds(StringUtils.join(filteredKbIds, ","));
            }
        }
        if (null != characterEditReq.getAudioConfig()) {
            one.setAudioConfig(characterEditReq.getAudioConfig());
        }
        return baseMapper.updateById(one) > 0;
    }

    @Transactional
    public boolean softDel(String uuid) {
        Character character = getOrThrow(uuid);
        characterPresetRelService.softDelBy(character.getUserId(), character.getId());
        return this.lambdaUpdate()
                .eq(Character::getId, character.getId())
                .set(Character::getIsDeleted, true)
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

    private Character getOrThrow(String uuid) {
        Character character = this.lambdaQuery()
                .eq(Character::getUuid, uuid)
                .eq(Character::getIsDeleted, false)
                .one();
        if (null == character) {
            throw new BaseException(A_CHARACTER_NOT_EXIST);
        }
        if (!character.getUserId().equals(ThreadContext.getCurrentUserId()) && !ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
        return character;
    }

    /**
     * 过滤出有效的MCP服务id列表 | Filter the list of valid MCP service IDs
     *
     * @param mcpIdsInReq 请求中传入的MCP服务id列表 | List of MCP service IDs passed in the request
     * @return 有效的MCP服务id列表 | List of valid MCP service IDs
     */
    private List<Long> filterEnableMcpIds(List<Long> mcpIdsInReq) {
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

    public List<Long> filterEnableKbIds(User user, List<Long> kbIdsInReq) {
        if (CollectionUtils.isEmpty(kbIdsInReq)) {
            return Collections.emptyList();
        }
        List<KbInfoResp> validKbList = filterEnableKb(user, kbIdsInReq);
        return validKbList.stream().map(KbInfoResp::getId).toList();
    }

    /**
     * 过滤出有效的知识库id列表 | Find the list of valid knowledge base IDs
     * 如果知识库是别人的且不是公开的，则不属于有效的可以关联的知识库
     *
     * @param user 当前用户 | Current user
     * @param ids  知识库id列表 | List of knowledge base IDs
     * @return 有效的知识库列表 | List of valid knowledge base
     */
    public List<KbInfoResp> filterEnableKb(User user, List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        return knowledgeBaseService.listByIds(ids).stream()
                .filter(item -> item.getIsPublic() || user.getUuid().equals(item.getOwnerUuid()))
                .toList();
    }

    private CharacterKnowledge convertToCharacterKbDto(User user, KbInfoResp kbInfo) {
        CharacterKnowledge result = new CharacterKnowledge();
        BeanUtils.copyProperties(kbInfo, result);
        result.setKbInfo(kbInfo);
        result.setIsMine(user.getUuid().equals(kbInfo.getOwnerUuid()));
        return result;
    }
}
