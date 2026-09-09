package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.entity.KnowledgeBaseAskRefGraph;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface KnowledgeBaseAskRefGraphMapper extends BaseMapper<KnowledgeBaseAskRefGraph> {
    List<KnowledgeBaseAskRefGraph> listByAskUuid(@Param("askUuid") String askUuid);
}
