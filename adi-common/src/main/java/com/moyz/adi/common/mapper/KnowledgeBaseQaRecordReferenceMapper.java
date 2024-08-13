package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseQaRecordReference;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseQaRecordReferenceMapper extends BaseMapper<KnowledgeBaseQaRecordReference> {
    List<KnowledgeBaseQaRecordReference> listByQaUuid(@Param("qaUuid") String qaUuid);
}
