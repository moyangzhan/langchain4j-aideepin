package com.moyz.adi.common.dto;

import com.baomidou.mybatisplus.annotation.TableField;
import com.moyz.adi.common.interfaces.AiModelAddGroup;
import com.moyz.adi.common.interfaces.AiModelEditGroup;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

import java.time.LocalDateTime;

@Validated
@Data
public class AiModelDto {

    @NotNull(groups = AiModelEditGroup.class)
    private Long id;

    @NotBlank(groups = AiModelAddGroup.class)
    private String type;

    @NotBlank(groups = AiModelAddGroup.class)
    private String name;

    private String setting;

    @NotBlank(groups = AiModelAddGroup.class)
    private String platform;

    private String remark;

    private Boolean isEnable;

    private Integer contextWindow;

    private Integer maxInputTokens;

    private Integer maxOutputTokens;

    private String inputTypes;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;

}
