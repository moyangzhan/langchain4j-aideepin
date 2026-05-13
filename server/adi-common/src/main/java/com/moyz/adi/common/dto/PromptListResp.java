package com.moyz.adi.common.dto;

import lombok.Data;

import java.util.List;

@Data
public class PromptListResp {
    private String maxUpdateTime;
    private List<PromptDto> prompts;
}
