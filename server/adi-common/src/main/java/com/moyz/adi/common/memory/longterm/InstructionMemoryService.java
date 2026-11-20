package com.moyz.adi.common.memory.longterm;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.memory.vo.BatchActionMemories;
import com.moyz.adi.common.memory.vo.InstructionCompressionDirectives;
import com.moyz.adi.common.service.CharacterMemoryInstructionService;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Instruction memory service: merge-candidate preparation and ADD / UPDATE / DELETE
 * execution against the relational rule table — no vector layer (rules are injected
 * in full, so retrieval has no consumer; the full alive rule set goes to the LLM
 * directly, which also catches paraphrase duplicates a similarity pre-filter would
 * miss). The relational row keeps the source message id for traceability. Does not
 * call LLMs and does not account for cost — the caller drives those.
 * <p>
 * 指令记忆服务：合并候选准备与 ADD / UPDATE / DELETE 执行，全部落在关系规则表——无向量层
 * （规则全量注入、检索无消费者；全量生效规则直给 LLM 判断，也更能抓住相似改述的重复——
 * 相似度预筛会漏）。关系行保留来源消息 id（规则学自哪轮对话）。不调 LLM、不计 token 成本，
 * 由调度器统筹这些事项。
 */
@Slf4j
@Service
public class InstructionMemoryService {

    @Resource
    private CharacterMemoryInstructionService characterMemoryInstructionService;

    /**
     * Prepare merge candidates for the new rules: every fact sees the FULL alive rule
     * set as its old memories (same list per fact; the LLM judges each new rule
     * against all of them). No vector search — the rule set is small and paraphrase
     * pairs can sit below any similarity threshold.
     * <p>
     * 为新规则准备合并候选：每条 fact 的旧记忆=全量生效规则集（各 fact 同一列表，LLM 对照
     * 全集判断）。不走向量检索——规则集规模小，且相似改述对的相似度可能低于任何阈值。
     */
    public MergeCandidates searchMergeCandidates(List<String> rules, Long characterId) {
        Map<String, List<Map<String, String>>> ruleToOldMemoriesForPrompt = new LinkedHashMap<>();
        Map<String, Map<String, String>> ruleToTmpIdToUuid = new HashMap<>();

        List<CharacterMemoryInstruction> aliveRules =
                characterMemoryInstructionService.listAllAliveByCharacter(characterId);
        List<Map<String, String>> oldMemories = new ArrayList<>();
        Map<String, String> tmpIdToUuid = new HashMap<>();
        int i = 0;
        for (CharacterMemoryInstruction rule : aliveRules) {
            String tmpId = String.valueOf(i);
            oldMemories.add(Map.of("id", tmpId, "text", rule.getContent()));
            tmpIdToUuid.put(tmpId, rule.getUuid());
            i++;
        }

        for (String rule : rules) {
            if (StringUtils.isBlank(rule)) {
                continue;
            }
            ruleToOldMemoriesForPrompt.put(rule, oldMemories);
            ruleToTmpIdToUuid.put(rule, tmpIdToUuid);
        }
        return new MergeCandidates(ruleToOldMemoriesForPrompt, ruleToTmpIdToUuid);
    }

    /**
     * Apply ADD / UPDATE / DELETE actions to instruction memory. Single-action failures
     * are caught here so other actions still proceed.
     * <p>
     * 应用 ADD / UPDATE / DELETE 操作。单条失败不影响其他操作。
     */
    public void applyActions(BatchActionMemories batchActions, Long characterId, Long userId,
                             Long sourceMsgId, MergeCandidates candidates) {
        if (batchActions == null || batchActions.getActions() == null) {
            return;
        }
        for (BatchActionMemories.BatchActionMemory action : batchActions.getActions()) {
            try {
                applyAction(action, characterId, userId, sourceMsgId, candidates);
            } catch (Exception ex) {
                log.warn("Apply instruction action failed, action={}, error={}", action, ex.getMessage());
            }
        }
    }

    private void applyAction(BatchActionMemories.BatchActionMemory action,
                             Long characterId,
                             Long userId,
                             Long sourceMsgId,
                             MergeCandidates candidates) {
        if (action == null || StringUtils.isBlank(action.getEvent())) {
            return;
        }
        String event = action.getEvent().trim().toUpperCase();
        if (AdiConstant.MemoryEvent.NONE.equalsIgnoreCase(event)) {
            log.info("No changes required for instruction id: {}", action.getId());
            return;
        }

        if (AdiConstant.MemoryEvent.ADD.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("ADD instruction action missing text, skip");
                return;
            }
            addRule(characterId, userId, sourceMsgId, action.getText());
            return;
        }

        Map<String, String> tmpIdToUuid = candidates.factToTmpIdToEmbeddingId.get(action.getFact());
        if (tmpIdToUuid == null) {
            log.warn("LLM hallucinated fact field, no matching context, fact={}, skip", action.getFact());
            return;
        }
        String tmpId = action.getId();
        if (StringUtils.isBlank(tmpId)) {
            log.warn("UPDATE/DELETE instruction action missing id, action={}, skip", action);
            return;
        }
        String uuid = tmpIdToUuid.get(tmpId);
        if (uuid == null) {
            log.warn("LLM hallucinated id, tmpId={} not in mapping, fact={}, skip", tmpId, action.getFact());
            return;
        }

        if (AdiConstant.MemoryEvent.DELETE.equalsIgnoreCase(event)) {
            characterMemoryInstructionService.softDeleteByUuid(uuid);
            return;
        }

        if (AdiConstant.MemoryEvent.UPDATE.equalsIgnoreCase(event)) {
            if (StringUtils.isBlank(action.getText())) {
                log.warn("UPDATE instruction action missing text, action={}, skip", action);
                return;
            }
            characterMemoryInstructionService.upsertContent(uuid, characterId, action.getText(), userId);
            return;
        }

        log.warn("Unknown instruction event: {}, action={}", event, action);
    }

    private void addRule(Long characterId, Long userId, Long sourceMsgId, String text) {
        CharacterMemoryInstruction row = new CharacterMemoryInstruction();
        row.setUuid(UuidUtil.createShort());
        row.setCharacterId(characterId);
        row.setContent(text);
        row.setSourceMsgId(sourceMsgId != null ? sourceMsgId : -1L);
        row.setUserId(userId);
        characterMemoryInstructionService.save(row);
    }

    /**
     * Apply uuid-keyed compression directives: a merge rewrites the kept row's content,
     * then drops the folded rows; a delete soft-deletes the row. Directives referencing
     * unknown uuids are ignored.
     * <p>
     * 应用按 uuid 编辑的压缩指令：合并=改写保留行内容、删除被并入行；删除=软删关系行。
     * 未知 uuid 的指令被忽略。
     */
    public void applyCompressionDirectives(InstructionCompressionDirectives directives,
                                           List<CharacterMemoryInstruction> aliveRules) {
        Map<String, CharacterMemoryInstruction> byUuid = aliveRules.stream()
                .collect(Collectors.toMap(CharacterMemoryInstruction::getUuid, Function.identity(), (a, b) -> a));
        if (directives == null) {
            return;
        }
        if (CollectionUtils.isNotEmpty(directives.getDeletes())) {
            for (String uuid : directives.getDeletes()) {
                if (byUuid.containsKey(uuid)) {
                    characterMemoryInstructionService.softDeleteByUuid(uuid);
                }
            }
        }
        if (CollectionUtils.isNotEmpty(directives.getMerges())) {
            for (InstructionCompressionDirectives.Merge merge : directives.getMerges()) {
                CharacterMemoryInstruction keepRow = merge.getKeep() == null ? null : byUuid.get(merge.getKeep());
                if (keepRow == null || StringUtils.isBlank(merge.getText())) {
                    continue;
                }
                characterMemoryInstructionService.upsertContent(merge.getKeep(),
                        keepRow.getCharacterId(), merge.getText(), keepRow.getUserId());
                if (CollectionUtils.isNotEmpty(merge.getDrop())) {
                    for (String uuid : merge.getDrop()) {
                        if (byUuid.containsKey(uuid) && !uuid.equals(merge.getKeep())) {
                            characterMemoryInstructionService.softDeleteByUuid(uuid);
                        }
                    }
                }
            }
        }
    }
}
