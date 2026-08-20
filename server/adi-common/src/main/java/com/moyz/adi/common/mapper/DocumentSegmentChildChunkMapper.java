package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.DocumentSegmentChildChunk;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface DocumentSegmentChildChunkMapper extends BaseMapper<DocumentSegmentChildChunk> {

    /**
     * 按向量库条目id批量累加命中次数（parent_child 模式子块）
     */
    void incrementHitCountByEmbeddingIds(@Param("embeddingIds") List<String> embeddingIds);
}
