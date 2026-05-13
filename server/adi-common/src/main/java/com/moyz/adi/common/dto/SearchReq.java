package com.moyz.adi.common.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class SearchReq {

    @NotBlank
    private String keyword;
}
