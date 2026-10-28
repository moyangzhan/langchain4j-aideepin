package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseMessageRefGraph;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseMessageRefGraphMapper extends BaseMapper<KnowledgeBaseMessageRefGraph> {
    List<KnowledgeBaseMessageRefGraph> listByChatUuid(@Param("chatUuid") String chatUuid);
}
