package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * LLM相关的统计
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TokenCostStatistic implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Integer todayTokenCost;
    private Integer monthTokenCost;
}
