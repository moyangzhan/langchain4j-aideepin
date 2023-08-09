package com.moyz.adi.common.dto;

import lombok.Data;

@Data
public class ConfigResp {
    private String secretKey;
    private Boolean contextEnable;
    private Integer contextMsgPairNum;
    private Integer quotaByTokenDaily;
    private Integer quotaByTokenMonthly;
    private Integer quotaByRequestDaily;
    private Integer quotaByRequestMonthly;
    private Integer quotaByImageDaily;
    private Integer quotaByImageMonthly;
    private Integer todayTokenCost;
    private Integer todayRequestTimes;
    private Integer todayGeneratedImageNumber;
    private Integer currMonthTokenCost;
    private Integer currMonthRequestTimes;
    private Integer currMonthGeneratedImageNumber;
}
