package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseChatRefGraph;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseChatRefGraphMapper extends BaseMapper<KnowledgeBaseChatRefGraph> {
    List<KnowledgeBaseChatRefGraph> listByChatUuid(@Param("chatUuid") String chatUuid);
}
