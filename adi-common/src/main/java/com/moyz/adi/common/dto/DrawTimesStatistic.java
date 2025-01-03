package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DrawTimesStatistic {
    private Integer todayDrawTimes;
    private Integer monthDrawTimes;
}
