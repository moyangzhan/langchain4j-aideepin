package com.moyz.adi.common.rag;

import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.chat.response.ChatResponseMetadata;
import dev.langchain4j.model.output.Response;

import java.util.List;
import java.util.function.Consumer;

/**
 * 组合ChatLanguageModel以提供更多功能
 */
public class AdiChatLanguageModelImpl implements ChatLanguageModel {

    private final ChatLanguageModel chatLanguageModel;
    private final Consumer<ChatResponse> consumer;

    public AdiChatLanguageModelImpl(ChatLanguageModel chatLanguageModel, Consumer<ChatResponse> consumer) {
        this.chatLanguageModel = chatLanguageModel;
        this.consumer = consumer;
    }

    @Override
    public ChatResponse chat(ChatMessage... messages) {
        ChatResponse chatResponse = ChatLanguageModel.super.chat(messages);
        consumer.accept(chatResponse);
        return chatResponse;
    }

    @Override
    public ChatResponse chat(List<ChatMessage> messages) {
        ChatResponse chatResponse = ChatLanguageModel.super.chat(messages);
        consumer.accept(chatResponse);
        return chatResponse;
    }

    @Override
    public Response<AiMessage> generate(List<ChatMessage> messages) {
        Response<AiMessage> response = chatLanguageModel.generate(messages);

        ChatResponse chatResponse = ChatResponse.builder()
                .aiMessage(response.content())
                .metadata(
                        ChatResponseMetadata.builder()
                                .tokenUsage(response.tokenUsage())
                                .finishReason(response.finishReason())
                                .build()
                )
                .build();
        consumer.accept(chatResponse);

        return response;
    }

    @Override
    public ChatResponse chat(ChatRequest chatRequest) {
        ChatResponse chatResponse = ChatLanguageModel.super.chat(chatRequest);
        consumer.accept(chatResponse);
        return chatResponse;
    }

}
