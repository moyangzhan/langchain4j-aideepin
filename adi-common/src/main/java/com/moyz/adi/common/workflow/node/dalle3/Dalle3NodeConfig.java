package com.moyz.adi.common.workflow.node.dalle3;

import lombok.Data;

@Data
public class Dalle3NodeConfig {
    private String prompt;
    private String size;
    private String quality;
}
