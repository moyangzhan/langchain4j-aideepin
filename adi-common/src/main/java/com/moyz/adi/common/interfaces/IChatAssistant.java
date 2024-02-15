package com.moyz.adi.common.interfaces;

import dev.langchain4j.service.SystemMessage;
import dev.langchain4j.service.TokenStream;
import dev.langchain4j.service.UserMessage;
import dev.langchain4j.service.V;

public interface IChatAssistant {

    @SystemMessage("{{sm}}")
    TokenStream chat(@V("sm") String systemMessage, @UserMessage String prompt);

    TokenStream chat(@UserMessage String prompt);
}
