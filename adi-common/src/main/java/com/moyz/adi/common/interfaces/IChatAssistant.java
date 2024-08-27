package com.moyz.adi.common.interfaces;

import dev.langchain4j.service.*;

public interface IChatAssistant {

    @SystemMessage("{{sm}}")
    TokenStream chatWith(@MemoryId String memoryId, @V("sm") String systemMessage, @UserMessage String prompt);

    @SystemMessage("{{sm}}")
    TokenStream chatWithSystem(@V("sm") String systemMessage, @UserMessage String prompt);

    TokenStream chatWithMemory(@MemoryId String memoryId, @UserMessage String prompt);

    TokenStream chatSimple(@UserMessage String prompt);

}
