package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.moyz.adi.common.dto.GraphContributionDto;
import com.moyz.adi.common.dto.KbEdgeDto;
import com.moyz.adi.common.entity.DocumentGraphEdge;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface DocumentGraphEdgeMapper extends BaseMapper<DocumentGraphEdge> {

    void insertIgnoreBatch(@Param("rows") List<DocumentGraphEdge> rows);

    List<GraphContributionDto> listWithExclusivity(@Param("segmentUuid") String segmentUuid);

    List<GraphContributionDto> listWithOutsideContributors(@Param("docUuid") String docUuid);

    int deleteBySegmentUuid(@Param("segmentUuid") String segmentUuid);

    int deleteByDocUuid(@Param("docUuid") String docUuid);

    List<KbEdgeDto> aggregateByDoc(@Param("docUuid") String docUuid, @Param("limit") int limit);

    long countByDocUuid(@Param("docUuid") String docUuid);
}
