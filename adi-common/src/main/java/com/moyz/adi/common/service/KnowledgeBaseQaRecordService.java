package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbQaRecordDto;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.KnowledgeBaseQaRecord;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordMapper;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.UUID;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;

@Slf4j
@Service
public class KnowledgeBaseQaRecordService extends ServiceImpl<KnowledgeBaseQaRecordMapper, KnowledgeBaseQaRecord> {

    @Resource
    private AiModelService aiModelService;

    public Page<KbQaRecordDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<KnowledgeBaseQaRecord> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseQaRecord::getKbUuid, kbUuid);
        wrapper.eq(KnowledgeBaseQaRecord::getIsDeleted, false);
        if (Boolean.FALSE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            wrapper.eq(KnowledgeBaseQaRecord::getUserId, ThreadContext.getCurrentUserId());
        }
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.like(KnowledgeBaseQaRecord::getQuestion, keyword);
        }
        wrapper.orderByDesc(KnowledgeBaseQaRecord::getUpdateTime);
        Page<KnowledgeBaseQaRecord> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);

        Page<KbQaRecordDto> result = new Page<>();
        MPPageUtil.convertToPage(page, result, KbQaRecordDto.class, (t1, t2) -> {
            AiModel aiModel = MODEL_ID_TO_OBJ.get(t1.getAiModelId());
            t2.setAiModelPlatform(null == aiModel ? "" : aiModel.getPlatform());
            return t2;
        });
        return result;
    }

    /**
     * 创建新的QA记录
     *
     * @param knowledgeBase 所属的知识库
     * @param question      用户的原始问题
     * @param prompt        根据{question}生成的最终提示词
     * @param promptTokens  提示词消耗的token
     * @param answer        答案
     * @param answerTokens  答案消耗的token
     * @param modelName     ai model name
     * @return
     */
    public KnowledgeBaseQaRecord createNewRecord(User user, KnowledgeBase knowledgeBase, String question, String prompt, int promptTokens, String answer, int answerTokens, String modelName) {
        String uuid = UUID.randomUUID().toString().replace("-", "");
        KnowledgeBaseQaRecord newObj = new KnowledgeBaseQaRecord();
        newObj.setKbId(knowledgeBase.getId());
        newObj.setKbUuid((knowledgeBase.getUuid()));
        newObj.setUuid(uuid);
        newObj.setUserId(user.getId());
        newObj.setQuestion(question);
        newObj.setPrompt(prompt);
        newObj.setPromptTokens(promptTokens);
        newObj.setAnswer(answer);
        newObj.setAnswerTokens(answerTokens);
        newObj.setAiModelId(aiModelService.getIdByName(modelName));
        baseMapper.insert(newObj);

        LambdaQueryWrapper<KnowledgeBaseQaRecord> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseQaRecord::getUuid, uuid);
        return baseMapper.selectOne(wrapper);
    }

    public boolean softDelete(String uuid) {
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            return ChainWrappers.lambdaUpdateChain(baseMapper)
                    .eq(KnowledgeBaseQaRecord::getUuid, uuid)
                    .set(KnowledgeBaseQaRecord::getIsDeleted, true)
                    .update();
        }
        KnowledgeBaseQaRecord exist = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getUuid, uuid)
                .one();
        if (null == exist) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseQaRecord::getId, exist.getId())
                .set(KnowledgeBaseQaRecord::getIsDeleted, true)
                .update();
    }
}
