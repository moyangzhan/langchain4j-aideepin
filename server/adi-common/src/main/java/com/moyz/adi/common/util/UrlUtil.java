package com.moyz.adi.common.util;

public class UrlUtil {
    public static String getUuid(String uuidWithExt) {
        String tmpUuid = uuidWithExt;
        int index = uuidWithExt.indexOf(".");
        if (index > 0) {
            tmpUuid = uuidWithExt.substring(0, index);
        }
        return tmpUuid;
    }
}
