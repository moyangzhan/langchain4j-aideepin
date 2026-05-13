package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RequestTimesStatistic implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Integer todayRequestTimes;
    private Integer monthRequestTimes;
}
