package com.moyz.adi.common.dto;

import lombok.Data;
import org.hibernate.validator.constraints.Length;
import org.springframework.validation.annotation.Validated;

import java.util.List;

@Data
@Validated
public class PromptsSaveReq {

    @Length(min = 1)
    private List<PromptDto> prompts;
}
