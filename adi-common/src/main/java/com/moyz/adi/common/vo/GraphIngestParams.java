package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.User;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.model.chat.ChatLanguageModel;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class GraphIngestParams {
    private User user;
    private Document document;
    private int overlap;
    private String tokenEstimator;
    private ChatLanguageModel chatLanguageModel;
    private List<String> identifyColumns;
    private List<String> appendColumns;
    private boolean isFreeToken;
}
