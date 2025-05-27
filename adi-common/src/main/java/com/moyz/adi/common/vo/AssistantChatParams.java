package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AssistantChatParams {
    private String memoryId;
    private String systemMessage;
    private String userMessage;
    //图片地址，多模态LLM才生效
    private List<String> imageUrls;
}
