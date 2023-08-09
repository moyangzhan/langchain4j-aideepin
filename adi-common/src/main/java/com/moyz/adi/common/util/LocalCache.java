package com.moyz.adi.common.util;

import com.moyz.adi.common.model.RequestRateLimit;

import java.util.HashMap;
import java.util.Map;

public class LocalCache {
    public static final Map<String, String> CONFIGS = new HashMap<>();

    public static RequestRateLimit TEXT_RATE_LIMIT_CONFIG;

    public static RequestRateLimit IMAGE_RATE_LIMIT_CONFIG;
}
