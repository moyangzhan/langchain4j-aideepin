package com.moyz.adi.common.languagemodel.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * OpenAI /images/edits 响应
 * <p>
 * Response of the OpenAI /images/edits endpoint.
 */
@Data
@NoArgsConstructor
public class OpenAiImageEditResp {

    private List<Item> data;

    @Data
    @NoArgsConstructor
    public static class Item {
        @JsonProperty("b64_json")
        private String b64Json;
        private String url;
    }
}
