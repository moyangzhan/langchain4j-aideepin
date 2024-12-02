package com.moyz.adi.common.rag;

import dev.langchain4j.service.AiServiceContext;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.spi.services.AiServicesFactory;

import java.util.Collection;

import static dev.langchain4j.spi.ServiceHelper.loadFactories;

public abstract class AdiAiServices<T> extends AiServices<T> {

    protected AdiAiServices(AiServiceContext context) {
        super(context);
    }

    public static <T> AiServices<T> builder(Class<T> aiService, int maxInputTokens) {
        AiServiceContext context = new AiServiceContext(aiService);
        Collection<AiServicesFactory> list = loadFactories(AiServicesFactory.class);
        if (list.iterator().hasNext()) {
            return list.iterator().next().create(context);
        }
        return new AdiDefaultAiServices<>(context, maxInputTokens);
    }
}
