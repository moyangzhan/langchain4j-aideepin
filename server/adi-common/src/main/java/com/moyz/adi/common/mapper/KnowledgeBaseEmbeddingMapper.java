package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseEmbeddingMapper extends BaseMapper<KnowledgeBaseEmbedding> {

    boolean deleteByItemUuid(@Param("kbItemUuid") String uuid, @Param("tableSuffix") String tableSuffix);

    boolean deleteByIds(@Param("ids") List<String> ids, @Param("tableSuffix") String tableSuffix);
}
