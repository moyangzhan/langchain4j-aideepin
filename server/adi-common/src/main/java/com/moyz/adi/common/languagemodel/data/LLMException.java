package com.moyz.adi.common.languagemodel.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class LLMException {
    private String code;
    private String message;
    private String type;
}
