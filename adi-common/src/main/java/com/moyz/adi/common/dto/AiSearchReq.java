package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Validated
@Data
public class AiSearchReq {

    @NotBlank
    private String searchText;

    private String engineName;

    private String modelName;

    private boolean briefSearch;
}
