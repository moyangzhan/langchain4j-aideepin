package com.moyz.adi.common.cosntant;

import java.util.List;

public class AdiConstant {

    public static final int DEFAULT_PAGE_SIZE = 1;

    /**
     * 验证码id过期时间：1小时
     */
    public static final int AUTH_CAPTCHA_ID_EXPIRE = 1;

    /**
     * 验证码过期时间，5分钟
     */
    public static final int AUTH_CAPTCHA_EXPIRE = 5;

    /**
     * 注册激活码有效时长，8小时
     */
    public static final int AUTH_ACTIVE_CODE_EXPIRE = 8;

    /**
     * token存活时间（8小时）
     */
    public static final int USER_TOKEN_EXPIRE = 8;

    public static final String DEFAULT_PASSWORD = "123456";

    public static final int LOGIN_MAX_FAIL_TIMES = 3;

    public static final String[] WEB_RESOURCES = {
            "/swagger-ui/index.html",
            "/swagger-ui",
            "/swagger-resources",
            "/v3/api-docs",
            "/favicon.ico",
            ".css",
            ".js",
            "/doc.html"
    };

    public static final int SECRET_KEY_TYPE_SYSTEM = 1;
    public static final int SECRET_KEY_TYPE_CUSTOM = 2;

    public static final String OPENAI_MESSAGE_DONE_FLAG = "[DONE]";

    public static final String DEFAULT_MODEL = "gpt-3.5-turbo";

    public static final String CREATE_IMAGE_RESP_FORMATS_B64JSON = "b64_json";
    public static final String OPENAI_CREATE_IMAGE_RESP_FORMATS_URL = "url";

    public static final List<String> OPENAI_CREATE_IMAGE_SIZES = List.of("256x256", "512x512", "1024x1024");



    public static class GenerateImage{
        public static final int INTERACTING_METHOD_GENERATE_IMAGE = 1;
        public static final int INTERACTING_METHOD_EDIT_IMAGE = 2;
        public static final int INTERACTING_METHOD_VARIATION = 3;

        public static final int STATUS_DOING = 1;
        public static final int STATUS_FAIL = 2;
        public static final int STATUS_SUCCESS = 3;
    }

    public static class SysConfigKey {
        public static final String SECRET_KEY = "secret_key";
        public static final String REQUEST_TEXT_RATE_LIMIT = "request_text_rate_limit";
        public static final String REQUEST_IMAGE_RATE_LIMIT = "request_image_rate_limit";
        public static final String CONVERSATION_MAX_NUM = "conversation_max_num";
        public static final String QUOTA_BY_TOKEN_DAILY = "quota_by_token_daily";
        public static final String QUOTA_BY_TOKEN_MONTHLY = "quota_by_token_monthly";
        public static final String QUOTA_BY_REQUEST_DAILY = "quota_by_request_daily";
        public static final String QUOTA_BY_REQUEST_MONTHLY = "quota_by_request_monthly";
        public static final String QUOTA_BY_IMAGE_DAILY = "quota_by_image_daily";
        public static final String QUOTA_BY_IMAGE_MONTHLY = "quota_by_image_monthly";

    }
}
