package com.moyz.adi.common.dto;

import com.moyz.adi.common.annotation.AskReqCheck;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import java.util.List;

@Schema(description = "对话的请求对象")
@Data
@AskReqCheck
public class AskReq {

    @Length(min = 32, max = 32)
    private String conversationUuid;

    private String parentMessageId;

    private String prompt;

    //语音聊天时产生的音频文件uuid
    private String audioUuid;
    //语音聊天时产生的音频时长，单位秒
    private Integer audioDuration;

    /**
     * 图片地址，多模态LLM使用，目前只支持本地图片uuid
     */
    private List<String> imageUrls;

    /**
     * If not empty, it means will request AI with the exist prompt, param {@code prompt} is ignored
     */
    private String regenerateQuestionUuid;

    private String modelPlatform;
    private String modelName;

    //后端用的临时变量
    private String processedPrompt;
}
