package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.spring.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.mapper.CharacterMemoryInstructionMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 指令记忆关系层服务——记忆内容的唯一事实源；向量表为派生索引，按 uuid 联接。
 * <p>
 * Relational service for instruction memory - the single source of truth for
 * memory content; the vector table is a derived index joined by uuid.
 */
@Slf4j
@Service
public class CharacterMemoryInstructionService extends ServiceImpl<CharacterMemoryInstructionMapper, CharacterMemoryInstruction> {

    /**
     * 全量注入的防御性上限：注入条数超过该值即视为规则集异常（merge 失效/提取失控），
     * 按更新时间倒序截断并告警。
     */
    public static final int INJECTION_LIMIT = 200;

    /**
     * 按记忆uuid查未软删行（弹窗反查 / 合并候选回表）
     * <p>
     * Map alive (not soft-deleted) rows by memory uuid (popup reverse lookup,
     * merge-candidate hydration).
     */
    public Map<String, CharacterMemoryInstruction> mapByUuids(List<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return Collections.emptyMap();
        }
        return lambdaQuery()
                .in(CharacterMemoryInstruction::getUuid, uuids)
                .eq(CharacterMemoryInstruction::getIsDeleted, false)
                .list()
                .stream()
                .collect(Collectors.toMap(CharacterMemoryInstruction::getUuid, Function.identity(), (a, b) -> a));
    }

    /**
     * 注入用全量规则：按更新时间倒序取全部生效规则，最多 {@value #INJECTION_LIMIT} 条；
     * 超限截断（最新调教优先）并告警——正常规则集为几十条量级，触顶即异常信号。
     * <p>
     * All alive rules for injection, newest update first, capped at
     * {@value #INJECTION_LIMIT}: overflow is truncated (most recent teachings win)
     * and warned — a healthy rule set is a few dozen entries; hitting the cap is an
     * anomaly signal.
     */
    public List<CharacterMemoryInstruction> listForInjection(Long characterId) {
        List<CharacterMemoryInstruction> rows = lambdaQuery()
                .eq(CharacterMemoryInstruction::getCharacterId, characterId)
                .eq(CharacterMemoryInstruction::getIsDeleted, false)
                .orderByDesc(CharacterMemoryInstruction::getUpdateTime)
                .orderByDesc(CharacterMemoryInstruction::getId)
                .last("limit " + (INJECTION_LIMIT + 1))
                .list();
        if (rows.size() > INJECTION_LIMIT) {
            log.warn("Instruction rules for character {} exceed the injection limit {}, truncated to newest {}",
                    characterId, rows.size(), INJECTION_LIMIT);
            return rows.subList(0, INJECTION_LIMIT);
        }
        return rows;
    }

    /**
     * 统计某角色未软删的规则条数（超限触发压缩用）
     * <p>
     * Count a character's alive rules (compression trigger threshold).
     */
    public long countAliveByCharacter(Long characterId) {
        return lambdaQuery()
                .eq(CharacterMemoryInstruction::getCharacterId, characterId)
                .eq(CharacterMemoryInstruction::getIsDeleted, false)
                .count();
    }

    /**
     * 列出某角色全部未软删规则（异步压缩的全量输入）
     * <p>
     * List all alive rules of a character (full input of the async compression).
     */
    public List<CharacterMemoryInstruction> listAllAliveByCharacter(Long characterId) {
        return lambdaQuery()
                .eq(CharacterMemoryInstruction::getCharacterId, characterId)
                .eq(CharacterMemoryInstruction::getIsDeleted, false)
                .list();
    }

    /**
     * 按记忆uuid upsert 内容（UPDATE 合并路径；行不存在时补插关系行，软删行复活）
     * <p>
     * Upsert content by memory uuid (merge UPDATE path; inserts the relational row
     * when absent, revives a soft-deleted row).
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
                .set(CharacterMemoryInstruction::getIsDeleted, true)
                .eq(CharacterMemoryInstruction::getUuid, uuid)
                .update();
    }
}
