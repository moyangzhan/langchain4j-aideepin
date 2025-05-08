package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseEmbeddingMapper extends BaseMapper<KnowledgeBaseEmbedding> {

    Page<KnowledgeBaseEmbedding> selectByItemUuid(Page<KnowledgeBaseEmbedding> page, @Param("kbItemUuid") String uuid, @Param("tableSuffix") String tableSuffix);

    boolean deleteByItemUuid(@Param("kbItemUuid") String uuid, @Param("tableSuffix") String tableSuffix);

    Integer countByKbUuid(@Param("kbUuid") String kbUuid, @Param("tableSuffix") String tableSuffix);
}
