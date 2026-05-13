package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.ConversationMemoryEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface ConversationMemoryEmbeddingMapper extends BaseMapper<ConversationMemoryEmbedding> {

    Page<ConversationMemoryEmbedding> selectByMsgId(Page<ConversationMemoryEmbedding> page, @Param("msgId") Long msgId, @Param("tableSuffix") String tableSuffix);

    boolean deleteByMsgId(@Param("msgId") Long msgId, @Param("tableSuffix") String tableSuffix);

    Integer countByConvId(@Param("convId") Long convId, @Param("tableSuffix") String tableSuffix);
}
