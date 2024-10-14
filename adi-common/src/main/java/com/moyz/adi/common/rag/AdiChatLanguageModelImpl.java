package com.moyz.adi.common.rag;

import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.output.Response;

import java.util.List;
import java.util.function.Consumer;

/**
 * 组合ChatLanguageModel以提供更多功能
 */
public class AdiChatLanguageModelImpl implements ChatLanguageModel {

    private ChatLanguageModel chatLanguageModel;
    private Consumer<Response<AiMessage>> consumer;

    public AdiChatLanguageModelImpl(ChatLanguageModel chatLanguageModel, Consumer<Response<AiMessage>> consumer) {
        this.chatLanguageModel = chatLanguageModel;
        this.consumer = consumer;
    }

    @Override
    public String generate(String userMessage) {
        return chatLanguageModel.generate(userMessage);
    }

    @Override
    public Response<AiMessage> generate(ChatMessage... messages) {
        Response<AiMessage> response = chatLanguageModel.generate(messages);
        consumer.accept(response);
        return response;
    }

    @Override
    public Response<AiMessage> generate(List<ChatMessage> messages) {
        Response<AiMessage> response = chatLanguageModel.generate(messages);
        consumer.accept(response);
        return response;
    }

    @Override
    public Response<AiMessage> generate(List<ChatMessage> messages, List<ToolSpecification> toolSpecifications) {
        Response<AiMessage> response = chatLanguageModel.generate(messages, toolSpecifications);
        consumer.accept(response);
        return response;
    }

    @Override
    public Response<AiMessage> generate(List<ChatMessage> messages, ToolSpecification toolSpecification) {
        Response<AiMessage> response = chatLanguageModel.generate(messages, toolSpecification);
        consumer.accept(response);
        return response;
    }
}
