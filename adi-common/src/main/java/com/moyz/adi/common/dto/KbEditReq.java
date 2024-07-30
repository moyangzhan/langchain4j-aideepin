package com.moyz.adi.common.dto;

import com.baomidou.mybatisplus.annotation.TableField;
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

    private Integer ragMaxResults;

    private Double ragMinScore;

    private Integer ragMaxOverlap;

    private Double llmTemperature;
}
