package com.moyz.adi.common.rag;

import dev.langchain4j.service.AiServiceContext;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.spi.services.AiServicesFactory;

import static dev.langchain4j.spi.ServiceHelper.loadFactories;

public abstract class AdiAiServices<T> extends AiServices<T> {

    protected AdiAiServices(AiServiceContext context) {
        super(context);
    }

    public static <T> AiServices<T> builder(Class<T> aiService, int maxInputTokens) {
        AiServiceContext context = new AiServiceContext(aiService);
        for (AiServicesFactory factory : loadFactories(AiServicesFactory.class)) {
            return factory.create(context);
        }
        return new AdiDefaultAiServices<>(context, maxInputTokens);
    }
}
