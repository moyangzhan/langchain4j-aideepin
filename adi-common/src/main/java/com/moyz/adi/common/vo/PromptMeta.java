package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PromptMeta {
    private Integer tokens;
    private String uuid;
}
