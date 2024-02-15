package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class CostStat {
    private int day;
    private int textRequestTimesByDay;
    private int textTokenCostByDay;
    private int imageGeneratedNumberByDay;
    private int textTokenCostByMonth;
    private int textRequestTimesByMonth;
    private int imageGeneratedNumberByMonth;
}
