package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.mapper.CharacterMemorySemanticMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 语义记忆关系层服务——记忆内容的唯一事实源；向量表为派生索引，按 uuid 联接。
 * <p>
 * Relational service for semantic memory - the single source of truth for memory
 * content; the vector table is a derived index joined by uuid.
 */
@Slf4j
@Service
public class CharacterMemorySemanticService extends ServiceImpl<CharacterMemorySemanticMapper, CharacterMemorySemantic> {

    /**
     * 按记忆uuid查未软删行（向量命中回表 / 弹窗反查 / 合并候选取文本）
     * <p>
     * Map alive (not soft-deleted) rows by memory uuid (hydration, popup reverse
     * lookup, merge-candidate text).
     */
    public Map<String, CharacterMemorySemantic> mapByUuids(List<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return Collections.emptyMap();
        }
        return lambdaQuery()
                .in(CharacterMemorySemantic::getUuid, uuids)
                .eq(CharacterMemorySemantic::getIsDeleted, false)
                .list()
                .stream()
                .collect(Collectors.toMap(CharacterMemorySemantic::getUuid, Function.identity(), (a, b) -> a));
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

    /**
     * 按记忆uuid upsert 内容（UPDATE 合并路径；行不存在时补插关系行）
     * <p>
     * Upsert content by memory uuid (merge UPDATE path; inserts the relational row
     * when absent).
     */
    public void upsertContent(String uuid, Long characterId, String content, Long userId) {
        baseMapper.upsertContent(uuid, characterId, content, userId != null ? userId : -1L);
    }

    /**
     * 按记忆uuid软删关系行（删除顺序：先移向量行，后软删本行）
     * <p>
     * Soft-delete the relational row by memory uuid (deletion order: remove the
     * vector row first, then soft-delete this row).
     */
    public void softDeleteByUuid(String uuid) {
        lambdaUpdate()
                .set(CharacterMemorySemantic::getIsDeleted, true)
                .eq(CharacterMemorySemantic::getUuid, uuid)
                .update();
    }
}
