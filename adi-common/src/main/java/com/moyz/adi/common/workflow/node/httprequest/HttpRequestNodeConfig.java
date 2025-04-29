package com.moyz.adi.common.workflow.node.httprequest;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

@Data
public class HttpRequestNodeConfig {
    @NotBlank
    private String method;
    @NotBlank
    private String url;
    @NotNull
    private List<Param> headers;
    private List<Param> params;

    /**
     * 当前只支持:text/plain,form-data,x-www-form-urlencoded,application/json
     * <br/>from-data暂时不支持上传文件
     */
    @NotNull
    @JsonProperty("content_type")
    private String contentType;
    @JsonProperty("text_body")
    private String textBody;
    @JsonProperty("form_data_body")
    private List<Param> formDataBody;
    @JsonProperty("form_urlencoded_body")
    private List<Param> formUrlencodedBody;
    @JsonProperty("json_body")
    private JsonNode jsonBody;
    @NotNull
    private Integer timeout;
    @NotNull
    @JsonProperty("retry_times")
    private Integer retryTimes;
    @JsonProperty("clear_html")
    private Boolean clearHtml;

    @Data
    public static class Param {
        private String name;
        private Object value;
    }

}
