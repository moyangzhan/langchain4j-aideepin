package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 会话统计信息
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConvStatistic implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer todayCreated;
    private Integer total;
}
