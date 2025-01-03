package com.moyz.adi.common.vo;

import lombok.Data;

/**
 * 费用统计
 */
@Data
public class CostStat {
    private int day;//天
    private int textRequestTimesByDay; // 每日文本请求次数
    private int textTokenCostByDay; // 每日文本令牌消耗
    private int drawTimesByDay; // 每日绘图次数
    private int textTokenCostByMonth; // 每月文本令牌消耗
    private int textRequestTimesByMonth; // 每月文本请求次数
    private int drawTimesByMonth; // 每月绘图次数
    private boolean isFree; // 是否免费
}
