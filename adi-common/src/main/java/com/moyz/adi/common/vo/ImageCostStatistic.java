package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ImageCostStatistic implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer todayCost;
    private Integer monthCost;
}
