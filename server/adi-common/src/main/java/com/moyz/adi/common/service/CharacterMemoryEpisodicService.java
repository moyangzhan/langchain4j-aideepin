package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.mapper.CharacterMemoryEpisodicMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 情景记忆关系层服务——记忆内容的唯一事实源；向量表为派生索引，按 uuid 联接。
 * <p>
 * Relational service for episodic memory - the single source of truth for memory
 * content; the vector table is a derived index joined by uuid.
 */
@Slf4j
@Service
public class CharacterMemoryEpisodicService extends ServiceImpl<CharacterMemoryEpisodicMapper, CharacterMemoryEpisodic> {

    /**
     * 按记忆uuid查未软删行（向量命中回表 / 弹窗反查）
     * <p>
     * Map alive (not soft-deleted) rows by memory uuid (hydration, popup reverse
     * lookup).
     */
    public Map<String, CharacterMemoryEpisodic> mapByUuids(List<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return Collections.emptyMap();
        }
        return lambdaQuery()
                .in(CharacterMemoryEpisodic::getUuid, uuids)
                .eq(CharacterMemoryEpisodic::getIsDeleted, false)
                .list()
                .stream()
                .collect(Collectors.toMap(CharacterMemoryEpisodic::getUuid, Function.identity(), (a, b) -> a));
    }

    /**
     * 时间轴检索：按创建时间倒序返回某角色最近 N 条情景事件（走 (character_id, create_time) 索引）
     * <p>
     * Timeline retrieval: a character's most recent episodic events, newest first
     * (served by the (character_id, create_time) index).
     */
    public List<CharacterMemoryEpisodic> listRecentByCharacter(Long characterId, int limit) {
        return lambdaQuery()
                .eq(CharacterMemoryEpisodic::getCharacterId, characterId)
                .eq(CharacterMemoryEpisodic::getIsDeleted, false)
                .orderByDesc(CharacterMemoryEpisodic::getCreateTime)
                .last("limit " + limit)
                .list();
    }

    /**
     * 按记忆uuid批量累加命中次数（软删行不计）
     * <p>
     * Increment hit counts by memory uuid in batch (soft-deleted rows excluded).
     */
    public void incrementHitCountByUuids(List<String> uuids) {
        if (CollectionUtils.isNotEmpty(uuids)) {
            baseMapper.incrementHitCountByUuids(uuids);
        }
    }
}
