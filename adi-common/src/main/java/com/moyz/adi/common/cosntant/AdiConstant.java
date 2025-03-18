package com.moyz.adi.common.cosntant;

import dev.langchain4j.model.input.PromptTemplate;

import java.util.List;

public class AdiConstant {
    private AdiConstant() {
    }

    public static final int DEFAULT_PAGE_SIZE = 10;

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

    public static final List<String> DALLE2_CREATE_IMAGE_SIZES = List.of("256x256", "512x512", "1024x1024");

    public static final List<String> DALLE3_CREATE_IMAGE_SIZES = List.of("1024x1024", "1024x1792", "1792x1024");

    public static final PromptTemplate PROMPT_TEMPLATE = PromptTemplate.from("""
            根据以下已知信息:
            {{information}}
            尽可能准确地回答用户的问题,以下是用户的问题:
            {{question}}
            注意,回答的内容不能让用户感知到已知信息的存在
            """);

    public static class ConversationConstant {
        private ConversationConstant() {
        }

        public static final String DEFAULT_NAME = "通用智能助手";
    }

    public static class GenerateImage {
        private GenerateImage() {
        }

        public static final int INTERACTING_METHOD_GENERATE_IMAGE = 1;
        public static final int INTERACTING_METHOD_EDIT_IMAGE = 2;
        public static final int INTERACTING_METHOD_VARIATION = 3;
        public static final int INTERACTING_METHOD_BACKGROUND_GENERATION = 4;

        public static final int STATUS_DOING = 1;
        public static final int STATUS_FAIL = 2;
        public static final int STATUS_SUCCESS = 3;
    }

    public static class MetadataKey {
        private MetadataKey() {
        }

        public static final String KB_UUID = "kb_uuid";
        public static final String KB_ITEM_UUID = "kb_item_uuid";
        public static final String ENGINE_NAME = "engine_name";
        public static final String SEARCH_UUID = "search_uuid";
    }

    public static class SysConfigKey {
        private SysConfigKey() {
        }

        public static final String DEEPSEEK_SETTING = "deepseek_setting";
        public static final String OPENAI_SETTING = "openai_setting";
        public static final String DASHSCOPE_SETTING = "dashscope_setting";
        public static final String QIANFAN_SETTING = "qianfan_setting";
        public static final String OLLAMA_SETTING = "ollama_setting";
        public static final String GOOGLE_SETTING = "google_setting";
        public static final String BING_SETTING = "bing_setting";
        public static final String BAIDU_SETTING = "baidu_setting";
        public static final String REQUEST_TEXT_RATE_LIMIT = "request_text_rate_limit";
        public static final String REQUEST_IMAGE_RATE_LIMIT = "request_image_rate_limit";
        public static final String CONVERSATION_MAX_NUM = "conversation_max_num";
        public static final String QUOTA_BY_TOKEN_DAILY = "quota_by_token_daily";
        public static final String QUOTA_BY_TOKEN_MONTHLY = "quota_by_token_monthly";
        public static final String QUOTA_BY_REQUEST_DAILY = "quota_by_request_daily";
        public static final String QUOTA_BY_REQUEST_MONTHLY = "quota_by_request_monthly";
        public static final String QUOTA_BY_IMAGE_DAILY = "quota_by_image_daily";
        public static final String QUOTA_BY_IMAGE_MONTHLY = "quota_by_image_monthly";
        public static final String QUOTA_BY_QA_ASK_DAILY = "quota_by_qa_ask_daily";
        public static final String STORAGE_LOCATION = "storage_location";
        public static final String STORAGE_LOCATION_ALI_OSS = "storage_location_ali_oss";
    }

    public static final String[] POI_DOC_TYPES = {"doc", "docx", "ppt", "pptx", "xls", "xlsx"};


    public static class ModelPlatform {
        private ModelPlatform() {
        }

        public static final String DEEPSEEK = "deepseek";
        public static final String OPENAI = "openai";
        public static final String DASHSCOPE = "dashscope";
        public static final String QIANFAN = "qianfan";
        public static final String OLLAMA = "ollama";
    }

    public static class ModelType {
        private ModelType() {
        }

        public static final String TEXT = "text";
        public static final String IMAGE = "image";
        public static final String EMBEDDING = "embedding";
        public static final String RERANK = "rerank";
    }

    public static class SearchEngineName {
        private SearchEngineName() {
        }

        public static final String GOOGLE = "google";
        public static final String BING = "bing";
        public static final String BAIDU = "baidu";
    }

    public static class SSEEventName {
        private SSEEventName() {
        }

        public static final String START = "[START]";
        public static final String DONE = "[DONE]";
        public static final String ERROR = "[ERROR]";
        public static final String META = "[META]";

        public static final String AI_SEARCH_SOURCE_LINKS = "[SOURCE_LINKS]";
        public static final String WF_NODE_CHUNK = "[WF_NODE_CHUNK]";
        public static final String WF_NODE_OUTPUT = "[WF_NODE_OUTPUT]";
    }

    public static final int RAG_TYPE_KB = 1;
    public static final int RAG_TYPE_SEARCH = 2;

    /**
     * 每块文档长度（按token算）
     */
    public static final int RAG_MAX_SEGMENT_SIZE_IN_TOKENS = 1000;

    /**
     * 文档召回默认数量
     */
    public static final int RAG_RETRIEVE_NUMBER_DEFAULT = 3;

    /**
     * 文档召回最大数量
     */
    public static final int RAG_RETRIEVE_NUMBER_MAX = 5;

    /**
     * 向量搜索时命中所需的最低分数
     */
    public static final double RAG_MIN_SCORE = 0.6;

    /**
     * 默认的最大输入token数
     */
    public static final int LLM_MAX_INPUT_TOKENS_DEFAULT = 4096;

    public static final String LLM_INPUT_TYPE_TEXT = "text";
    public static final String LLM_INPUT_TYPE_IMAGE = "image";
    public static final String LLM_INPUT_TYPE_AUDIO = "audio";
    public static final String LLM_INPUT_TYPE_VIDEO = "video";

    public static final String[] GRAPH_ENTITY_EXTRACTION_ENTITY_TYPES = {"organization", "person", "geo", "event"};
    public static final String GRAPH_TUPLE_DELIMITER = "<|>";
    public static final String GRAPH_RECORD_DELIMITER = "##";
    public static final String GRAPH_COMPLETION_DELIMITER = "<|COMPLETE|>";

    /**
     * 唯一标识字段，如果该字段有指定，则根据该配置判断Vertex或Edge是否唯一，如知识库中根据 name、metadata->>kb_uuid 来做判断
     */
    public static final String GRAPH_METADATA_IDENTIFY_COLUMNS = "graph_metadata_identify_columns";

    /**
     * 内容追加字段
     * 更新数据时，如遇到该标识中的字段，追加内容而不是替换
     */
    public static final String GRAPH_METADATA_APPEND_COLUMNS = "graph_metadata_append_columns_if_exist";

    public static final int AI_IMAGE_TYPE_REGULAR = 1;
    public static final int AI_IMAGE_TYPE_THUMBNAIL = 2;
    public static final int AI_IMAGE_TYPE_REGULAR_MARK = 3;
    public static final int AI_IMAGE_TYPE_THUMBNAIL_MARK = 4;

    public static final String DOC_INDEX_TYPE_EMBEDDING = "embedding";
    public static final String DOC_INDEX_TYPE_GRAPHICAL = "graphical";

    public static final String DRAW_TYPE_PUBLIC = "public";
    public static final String DRAW_TYPE_STARRED = "starred";
    public static final String DRAW_TYPE_MINE = "mine";

    public static final String MP_LIMIT_1 = "limit 1";

    /**
     * 文件存储在本地
     */
    public static final int STORAGE_LOCATION_LOCAL = 1;

    /**
     * 文件存储到阿里云OSS
     */
    public static final int STORAGE_LOCATION_ALI_OSS = 2;

    public static final String URL_PREFIX_FILE = "/file/";
    public static final String URL_PREFIX_IMAGE = "/image/";
    public static final String URL_PREFIX_MY_IMAGE = "/my-image/";
    public static final String URL_PREFIX_MY_THUMBNAIL = "/my-thumbnail/";

    public static final List<String> IMAGE_EXTENSIONS = List.of("jpg", "jpeg", "png", "gif", "bmp", "webp");

    public static final String W_FAILED = "FAILED";

    public static class WorkflowConstant {
        public static final String DEFAULT_INPUT_PARAM_NAME = "input";
        public static final String DEFAULT_OUTPUT_PARAM_NAME = "output";

        public static final int NODE_PROCESS_STATUS_READY = 1;
        public static final int NODE_PROCESS_STATUS_DOING = 2;
        public static final int NODE_PROCESS_STATUS_SUCCESS = 3;
        public static final int NODE_PROCESS_STATUS_FAIL = 4;

        public static final int WORKFLOW_PROCESS_STATUS_READY = 1;
        public static final int WORKFLOW_PROCESS_STATUS_DOING = 2;
        public static final int WORKFLOW_PROCESS_STATUS_SUCCESS = 3;
        public static final int WORKFLOW_PROCESS_STATUS_FAIL = 4;

        public static final int WORKFLOW_NODE_PROCESS_TYPE_NORMAL = 1;
        public static final int WORKFLOW_NODE_PROCESS_TYPE_CONDITIONAL = 2;
        public static final int WORKFLOW_NODE_PROCESS_TYPE_PARALLEL = 3;
    }
}
