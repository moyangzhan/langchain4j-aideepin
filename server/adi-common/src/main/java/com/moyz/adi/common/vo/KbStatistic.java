package com.moyz.adi.common.vo;

import lombok.Data;

import java.io.Serializable;

/**
 * 知识库的统计信息
 */
@Data
public class KbStatistic implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer kbTodayCreated;
    private Integer itemTodayCreated;
    private Integer kbTotal;
    private Integer itemTotal;
}
