package com.moyz.adi.common.interfaces;

import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.service.MemoryId;
import dev.langchain4j.service.SystemMessage;
import dev.langchain4j.service.UserMessage;
import dev.langchain4j.service.V;

import java.util.List;

public interface IChatAssistant {

    @SystemMessage("{{sm}}")
    String chatWithSystem(@MemoryId String memoryId, @V("sm") String systemMessage, @UserMessage String prompt, @UserMessage List<ImageContent> images);

    String chat(@MemoryId String memoryId, @UserMessage String prompt, @UserMessage List<ImageContent> images);

}
