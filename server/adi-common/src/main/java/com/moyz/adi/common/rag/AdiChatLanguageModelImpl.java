package com.moyz.adi.common.rag;

import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;

import java.util.List;
import java.util.function.Consumer;

/**
 * 组合ChatModel以提供更多功能
 */
public class AdiChatLanguageModelImpl implements ChatModel {

    private final Consumer<ChatResponse> consumer;

    public AdiChatLanguageModelImpl(ChatModel ChatModel, Consumer<ChatResponse> consumer) {
        this.consumer = consumer;
    }

    @Override
    public ChatResponse chat(ChatMessage... messages) {
        ChatResponse chatResponse = ChatModel.super.chat(messages);
        consumer.accept(chatResponse);
        return chatResponse;
    }

    @Override
    public ChatResponse chat(List<ChatMessage> messages) {
        ChatResponse chatResponse = ChatModel.super.chat(messages);
        consumer.accept(chatResponse);
        return chatResponse;
    }

    @Override
    public ChatResponse chat(ChatRequest chatRequest) {
        ChatResponse chatResponse = ChatModel.super.chat(chatRequest);
        consumer.accept(chatResponse);
        return chatResponse;
    }

}
