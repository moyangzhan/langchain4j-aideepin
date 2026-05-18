package com.moyz.adi.common.dto;

import com.moyz.adi.common.vo.*;
import lombok.Data;

@Data
public class StatisticDto {
    private UserStatistic userStatistic;
    private KbStatistic kbStatistic;
    private TokenCostStatistic tokenCostStatistic;
    private CharacterStatistic characterStatistic;
    private ImageCostStatistic imageCostStatistic;
}
