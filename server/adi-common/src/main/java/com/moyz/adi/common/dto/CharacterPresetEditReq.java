package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class CharacterPresetEditReq {

    @NotBlank
    private String title;
    @NotBlank
    private String remark;
    @NotBlank
    private String aiSystemMessage;
    private String kbTitle;
    private String type;
}
