package com.moyz.adi.common.interfaces;

import dev.langchain4j.service.*;

public interface IChatAssistantWithoutMemory {

    @SystemMessage("{{sm}}")
    TokenStream chat(@V("sm") String systemMessage, @UserMessage String prompt);

    TokenStream chatWithoutSystemMessage(@UserMessage String prompt);
}
