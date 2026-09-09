package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseAskRefEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseAskRefEmbeddingMapper extends BaseMapper<KnowledgeBaseAskRefEmbedding> {
    List<KnowledgeBaseAskRefEmbedding> listByAskUuid(@Param("askUuid") String askUuid);
}
