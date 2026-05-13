package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.ConversationMessageRefEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface ConversationMessageRefEmbeddingMapper extends BaseMapper<ConversationMessageRefEmbedding> {
    List<ConversationMessageRefEmbedding> listByMsgUuid(@Param("uuid") String uuid);
}
