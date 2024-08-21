package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.*;
import lombok.Data;

@Data
public class StatisticDto {
    private UserStatistic userStatistic;
    private KbStatistic kbStatistic;
    private TokenCostStatistic tokenCostStatistic;
    private ConvStatistic convStatistic;
    private ImageCostStatistic imageCostStatistic;
}
