package com.moyz.adi.common.workflow;

import lombok.Getter;

import java.util.Arrays;

@Getter
public enum WfComponentNameEnum {
    START("Start"),

    END("End"),

    LLM_ANSWER("Answer"),

    IMAGE_MODEL("Draw"),

    DOCUMENT_EXTRACTOR("DocumentExtractor"),

    KEYWORD_EXTRACTOR("KeywordExtractor"),

    KNOWLEDGE_RETRIEVER("KnowledgeRetrieval"),

    SWITCHER("Switcher"),

    CLASSIFIER("Classifier"),

    TEMPLATE("Template"),

    GOOGLE_SEARCH("Google");

    private final String name;

    WfComponentNameEnum(String name) {
        this.name = name;
    }

    public static WfComponentNameEnum getByName(String name) {
        return Arrays.stream(WfComponentNameEnum.values()).filter(item -> item.name.equals(name)).findFirst().orElse(null);
    }
}
