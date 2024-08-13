package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.KnowledgeBaseQaRecordReference;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordReferenceMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class KnowledgeBaseQaRecordReferenceService extends ServiceImpl<KnowledgeBaseQaRecordReferenceMapper, KnowledgeBaseQaRecordReference> {

    public List<KnowledgeBaseQaRecordReference> listByQaUuid(String aqRecordUuid){
        return this.getBaseMapper().listByQaUuid(aqRecordUuid);
    }
}
