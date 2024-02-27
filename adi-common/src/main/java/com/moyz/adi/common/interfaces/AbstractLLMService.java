package com.moyz.adi.common.interfaces;

import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.ChatMeta;
import com.moyz.adi.common.vo.QuestionMeta;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.net.Proxy;
import java.util.UUID;

@Slf4j
public abstract class AbstractLLMService<T> {

    protected Proxy proxy;

    protected String modelName;

    protected T setting;

    protected StreamingChatLanguageModel streamingChatLanguageModel;
    protected ChatLanguageModel chatLanguageModel;

    public AbstractLLMService(String modelName, String settingName, Class<T> clazz, Proxy proxy){
        this.modelName = modelName;
        this.proxy = proxy;
        String st = LocalCache.CONFIGS.get(settingName);
        setting = JsonUtil.fromJson(st, clazz);
    }

    /**
     * 检测该service是否可用（不可用的情况通常是没有配置key）
     *
     * @return
     */
    public abstract boolean isEnabled();

    public ChatLanguageModel getChatLLM() {
        if (null != chatLanguageModel) {
            return chatLanguageModel;
        }
        chatLanguageModel = buildChatLLM();
        return chatLanguageModel;
    }

    public StreamingChatLanguageModel getStreamingChatLLM() {
        if (null != streamingChatLanguageModel) {
            return streamingChatLanguageModel;
        }
        streamingChatLanguageModel = buildStreamingChatLLM();
        return streamingChatLanguageModel;
    }

    protected abstract ChatLanguageModel buildChatLLM();

    protected abstract StreamingChatLanguageModel buildStreamingChatLLM();

    public String chat(ChatMessage chatMessage) {
        return getChatLLM().generate(chatMessage).content().text();
    }

    public void sseChat(SseAskParams params, TriConsumer<String, QuestionMeta, AnswerMeta> consumer) {

        //create chat assistant
        AiServices<IChatAssistant> serviceBuilder = AiServices.builder(IChatAssistant.class)
                .streamingChatLanguageModel(getStreamingChatLLM());
        if (null != params.getChatMemory()) {
            serviceBuilder.chatMemory(params.getChatMemory());
        }
        IChatAssistant chatAssistant = serviceBuilder.build();

        TokenStream tokenStream;
        if (StringUtils.isNotBlank(params.getSystemMessage())) {
            tokenStream = chatAssistant.chat(params.getSystemMessage(), params.getUserMessage());
        } else {
            tokenStream = chatAssistant.chat(params.getUserMessage());
        }
        tokenStream.onNext((content) -> {
                    log.info("get content:{}", content);
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    try {
                        params.getSseEmitter().send(" " + content);
                    } catch (IOException e) {
                        log.error("stream onNext error", e);
                    }
                })
                .onComplete((response) -> {
                    log.info("返回数据结束了:{}", response);
                    String questionUuid = StringUtils.isNotBlank(params.getRegenerateQuestionUuid()) ? params.getRegenerateQuestionUuid() : UUID.randomUUID().toString().replace("-", "");
                    QuestionMeta questionMeta = new QuestionMeta(response.tokenUsage().inputTokenCount(), questionUuid);
                    AnswerMeta answerMeta = new AnswerMeta(response.tokenUsage().outputTokenCount(), UUID.randomUUID().toString().replace("-", ""));
                    ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
                    String meta = JsonUtil.toJson(chatMeta).replaceAll("\r\n", "");
                    log.info("meta:" + meta);
                    try {
                        params.getSseEmitter().send(" [META]" + meta);
                    } catch (IOException e) {
                        log.error("stream onComplete error", e);
                        throw new RuntimeException(e);
                    }
                    // close eventSourceEmitter after tokens was calculated
                    params.getSseEmitter().complete();
                    consumer.accept(response.content().text(), questionMeta, answerMeta);
                })
                .onError((error) -> {
                    log.error("stream error", error);
                    try {
                        params.getSseEmitter().send(SseEmitter.event().name("error").data(error.getMessage()));
                    } catch (IOException e) {
                        log.error("sse error", e);
                    }
                    params.getSseEmitter().complete();
                })
                .start();
    }

}
