package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface DocumentSegmentQuestionMapper extends BaseMapper<DocumentSegmentQuestion> {

    /**
     * 按向量库条目id批量累加命中次数（qa 模式问题）
     */
    void incrementHitCountByEmbeddingIds(@Param("embeddingIds") List<String> embeddingIds);
}
