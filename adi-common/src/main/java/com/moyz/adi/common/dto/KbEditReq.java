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

    private Long ingestModelId;

    private Double queryLlmTemperature;

    private String querySystemMessage;
}
