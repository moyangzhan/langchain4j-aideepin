package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.DocumentSegment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface DocumentSegmentMapper extends BaseMapper<DocumentSegment> {

    /**
     * 按向量库条目id批量累加命中次数（text 模式段）
     */
    void incrementHitCountByEmbeddingIds(@Param("embeddingIds") List<String> embeddingIds);

    /**
     * 按段id批量累加命中次数（qa 答案 / parent_child 父段的传导累加）
     */
    void incrementHitCountByIds(@Param("ids") List<Long> ids);
}
