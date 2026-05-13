package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class RequestRateLimit {
    private int times;
    private int minutes;
    private int type;

    public static final int TYPE_TEXT = 1;
    public static final int TYPE_IMAGE = 2;
}
