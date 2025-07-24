package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.User;
import lombok.Data;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;


@Data
public class SseAskParams {

    //请求标识,如:知识库的记录uuid,搜索记录uuid
    private String uuid;

    private String modelName;

    private User user;

    private String regenerateQuestionUuid;

    /**
     * 2:text,3:audio
     */
    private Integer answerContentType;

    private SseEmitter sseEmitter;

    /**
     * 组装LLMService所属的属性，非必填
     */
    private LLMBuilderProperties llmBuilderProperties;

    /**
     * 最终提交给llm的信息，必填
     */
    private ChatModelParams chatModelParams;
}
