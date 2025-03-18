package com.moyz.adi.common.util;


import dev.langchain4j.community.model.dashscope.QwenModelName;

import java.util.stream.Stream;

public class DashscopeUtil {
    private DashscopeUtil(){}
    public static Stream<String> languageModelNameProvider() {
        return Stream.of(
                QwenModelName.QWEN_TURBO,
                QwenModelName.QWEN_PLUS,
                QwenModelName.QWEN_MAX,
                QwenModelName.QWEN_MAX_LONGCONTEXT,
                QwenModelName.QWEN_7B_CHAT,
                QwenModelName.QWEN_14B_CHAT,
                QwenModelName.QWEN_72B_CHAT,
                QwenModelName.QWEN1_5_7B_CHAT,
                QwenModelName.QWEN1_5_14B_CHAT,
                QwenModelName.QWEN1_5_32B_CHAT,
                QwenModelName.QWEN1_5_72B_CHAT,
                QwenModelName.QWEN2_0_5B_INSTRUCT,
                QwenModelName.QWEN2_1_5B_INSTRUCT,
                QwenModelName.QWEN2_7B_INSTRUCT,
                QwenModelName.QWEN2_72B_INSTRUCT,
                QwenModelName.QWEN2_57B_A14B_INSTRUCT,
                QwenModelName.QWEN2_5_0_5B_INSTRUCT,
                QwenModelName.QWEN2_5_1_5B_INSTRUCT,
                QwenModelName.QWEN2_5_3B_INSTRUCT,
                QwenModelName.QWEN2_5_7B_INSTRUCT,
                QwenModelName.QWEN2_5_14B_INSTRUCT,
                QwenModelName.QWEN2_5_32B_INSTRUCT,
                QwenModelName.QWEN2_5_72B_INSTRUCT
        );
    }

    public static Stream<String> nonMultimodalChatModelNameProvider() {
        return Stream.of(
                QwenModelName.QWEN_TURBO,
                QwenModelName.QWEN_PLUS,
                QwenModelName.QWEN_MAX,
                QwenModelName.QWEN_MAX_LONGCONTEXT,
                QwenModelName.QWEN_7B_CHAT,
                QwenModelName.QWEN_14B_CHAT,
                QwenModelName.QWEN_72B_CHAT,
                QwenModelName.QWEN1_5_7B_CHAT,
                QwenModelName.QWEN1_5_14B_CHAT,
                QwenModelName.QWEN1_5_32B_CHAT,
                QwenModelName.QWEN1_5_72B_CHAT,
                QwenModelName.QWEN2_0_5B_INSTRUCT,
                QwenModelName.QWEN2_1_5B_INSTRUCT,
                QwenModelName.QWEN2_7B_INSTRUCT,
                QwenModelName.QWEN2_72B_INSTRUCT,
                QwenModelName.QWEN2_57B_A14B_INSTRUCT,
                QwenModelName.QWEN2_5_0_5B_INSTRUCT,
                QwenModelName.QWEN2_5_1_5B_INSTRUCT,
                QwenModelName.QWEN2_5_3B_INSTRUCT,
                QwenModelName.QWEN2_5_7B_INSTRUCT,
                QwenModelName.QWEN2_5_14B_INSTRUCT,
                QwenModelName.QWEN2_5_32B_INSTRUCT,
                QwenModelName.QWEN2_5_72B_INSTRUCT
        );
    }

    public static Stream<String> functionCallChatModelNameProvider() {
        return Stream.of(QwenModelName.QWEN_MAX);
    }

    public static Stream<String> vlChatModelNameProvider() {
        return Stream.of(
                QwenModelName.QWEN_VL_PLUS,
                QwenModelName.QWEN_VL_MAX);
    }

    public static Stream<String> audioChatModelNameProvider() {
        return Stream.of(
                QwenModelName.QWEN_AUDIO_CHAT,
                QwenModelName.QWEN2_AUDIO_INSTRUCT);
    }

    public static Stream<String> embeddingModelNameProvider() {
        return Stream.of(
                QwenModelName.TEXT_EMBEDDING_V1,
                QwenModelName.TEXT_EMBEDDING_V2
        );
    }
}
