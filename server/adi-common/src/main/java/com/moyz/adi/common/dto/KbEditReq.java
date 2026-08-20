package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class KbEditReq {

    private Long id;

    private String uuid;

    @NotBlank
    private String title;

    private String remark;

    private Boolean isPublic;

    private Boolean isStrict;

    private Integer retrieveMaxResults;

    private Double retrieveMinScore;

    private Integer ingestMaxOverlap;

    private String ingestSplitStrategy;

    private Integer ingestMaxSegmentSize;

    private String ingestCustomSeparator;

    /**
     * 父子模式子块最大token数（KB级）
     */
    private Integer ingestChildMaxSegmentSize;

    private Long ingestModelId;

    private String ingestTokenEstimator;

    private Double queryLlmTemperature;

    private String querySystemMessage;
}
