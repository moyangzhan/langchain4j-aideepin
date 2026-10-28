package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseMessageRefEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseMessageRefEmbeddingMapper extends BaseMapper<KnowledgeBaseMessageRefEmbedding> {
    List<KnowledgeBaseMessageRefEmbedding> listByChatUuid(@Param("chatUuid") String chatUuid);
}
