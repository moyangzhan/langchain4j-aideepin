package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;

public class SearchEngineUtil {

    public static boolean checkGoogleCountry(String country) {
        boolean result = false;
        for (String googleCountry : AdiConstant.SearchEngineName.GOOGLE_COUNTRIES) {
            if (googleCountry.equalsIgnoreCase(country)) {
                result = true;
                break;
            }
        }
        return result;
    }

    public static boolean checkGoogleLanguage(String language) {
        boolean result = false;
        for (String googleCountry : AdiConstant.SearchEngineName.GOOGLE_LANGUAGES) {
            if (googleCountry.equalsIgnoreCase(language)) {
                result = true;
                break;
            }
        }
        return result;
    }
}
