package com.moyz.adi.common.interfaces;

import dev.langchain4j.service.*;

public interface IChatAssistant {

    @SystemMessage("{{sm}}")
    TokenStream chat(@MemoryId String memoryId, @V("sm") String systemMessage, @UserMessage String prompt);

    TokenStream chatWithoutSystemMessage(@MemoryId String memoryId, @UserMessage String prompt);
}
