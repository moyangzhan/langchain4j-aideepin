package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 配置信息
 */
@Data
public class ConfigResp {
    private Integer contextMsgPairNum;
    private UserQuota userQuota;
    private CostStatResp quotaCost;
}
