package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.ConversationMessageRefMemoryEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface ConversationMessageRefMemoryEmbeddingMapper extends BaseMapper<ConversationMessageRefMemoryEmbedding> {
    List<ConversationMessageRefMemoryEmbedding> listByMsgUuid(@Param("uuid") String msgUuid);
}
