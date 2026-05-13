package com.moyz.adi.common.interfaces;

import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.service.SystemMessage;
import dev.langchain4j.service.UserMessage;
import dev.langchain4j.service.V;

import java.util.List;

/**
 * 临时聊天助手（不携带上下文）
 */
public interface ITempChatAssistant {

    @SystemMessage("{{sm}}")
    String chatWithSystem(@V("sm") String systemMessage, @UserMessage String prompt, @UserMessage List<ImageContent> images);

    String chatSimple(@UserMessage String prompt, @UserMessage List<ImageContent> images);

}
