package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.ConvStatistic;
import com.moyz.adi.common.vo.KbStatistic;
import com.moyz.adi.common.vo.TokenCostStatistic;
import com.moyz.adi.common.vo.UserStatistic;
import lombok.Data;

@Data
public class StatisticDto {
    private UserStatistic userStatistic;
    private KbStatistic kbStatistic;
    private TokenCostStatistic tokenCostStatistic;
    private ConvStatistic convStatistic;
}
