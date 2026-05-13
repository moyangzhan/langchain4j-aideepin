package com.moyz.adi.common.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserQuota {
    private Integer tokenByDay;
    private Integer tokenByMonth;
    private Integer requestTimesByDay;
    private Integer requestTimesByMonth;
    private Integer drawByDay;
    private Integer drawByMonth;
}
