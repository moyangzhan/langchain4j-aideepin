package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.KnowledgeBaseQaRefEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordReferenceMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class KnowledgeBaseQaRecordReferenceService extends ServiceImpl<KnowledgeBaseQaRecordReferenceMapper, KnowledgeBaseQaRefEmbedding> {

    public List<KnowledgeBaseQaRefEmbedding> listByQaUuid(String aqRecordUuid){
        return this.getBaseMapper().listByQaUuid(aqRecordUuid);
    }
}
