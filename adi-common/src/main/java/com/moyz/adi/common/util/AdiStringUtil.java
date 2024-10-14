package com.moyz.adi.common.util;

import org.jsoup.Jsoup;

public class AdiStringUtil {
    public static String clearStr(String str) {
        org.jsoup.nodes.Document doc = Jsoup.parse(str);
        return doc.text();
    }
}
