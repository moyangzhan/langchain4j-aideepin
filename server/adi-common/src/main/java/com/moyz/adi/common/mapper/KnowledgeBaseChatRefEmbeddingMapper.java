package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseChatRefEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseChatRefEmbeddingMapper extends BaseMapper<KnowledgeBaseChatRefEmbedding> {
    List<KnowledgeBaseChatRefEmbedding> listByChatUuid(@Param("chatUuid") String chatUuid);
}
