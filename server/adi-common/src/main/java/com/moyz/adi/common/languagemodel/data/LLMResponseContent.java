package com.moyz.adi.common.languagemodel.data;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class LLMResponseContent {

    private String thinkingContent;

    /**
     * 响应文件内容
     */
    private String content;

    /**
     * 由响应文件生成的音频文件路径，本地绝对路径或OSS url
     * ps: 文件可能不存在
     */
    private String audioPath;

}
