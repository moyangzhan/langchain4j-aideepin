package com.moyz.adi.common.interfaces;

import dev.langchain4j.data.message.ImageContent;
import dev.langchain4j.service.*;

import java.util.List;

/**
 * 临时聊天助手-流式输出（不携带上下文）
 */
public interface ITempStreamingChatAssistant {

    @SystemMessage("{{sm}}")
    TokenStream chatWithSystem(@V("sm") String systemMessage, @UserMessage String prompt, @UserMessage List<ImageContent> images);

    TokenStream chatSimple(@UserMessage String prompt, @UserMessage List<ImageContent> images);

}
