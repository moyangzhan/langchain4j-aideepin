package com.moyz.adi.common.vo;

import lombok.Data;

import java.io.Serializable;

/**
 * LLM相关的统计
 */
@Data
public class TokenCostStatistic implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long todayTokenCost;
    private Long monthTokenCost;
}
