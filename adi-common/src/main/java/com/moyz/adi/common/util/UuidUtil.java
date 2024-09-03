package com.moyz.adi.common.util;

import java.util.UUID;

public class UuidUtil {
    public static String createShort() {
        return UUID.randomUUID().toString().replace("-", "");
    }
}
