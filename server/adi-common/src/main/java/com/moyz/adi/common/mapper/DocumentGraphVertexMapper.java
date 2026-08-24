package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.dto.GraphContributionDto;
import com.moyz.adi.common.dto.KbVertexDto;
import com.moyz.adi.common.entity.DocumentGraphVertex;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface DocumentGraphVertexMapper extends BaseMapper<DocumentGraphVertex> {

    void insertIgnoreBatch(@Param("rows") List<DocumentGraphVertex> rows);

    List<GraphContributionDto> listWithExclusivity(@Param("segmentUuid") String segmentUuid);

    List<GraphContributionDto> listWithOutsideContributors(@Param("docUuid") String docUuid);

    int deleteBySegmentUuid(@Param("segmentUuid") String segmentUuid);

    int deleteByDocUuid(@Param("docUuid") String docUuid);

    List<KbVertexDto> aggregateByDoc(@Param("docUuid") String docUuid, @Param("limit") int limit);

    long countByDocUuid(@Param("docUuid") String docUuid);
}
