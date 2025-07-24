package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ChatMeta {
    private PromptMeta question;
    private AnswerMeta answer;
    private AudioInfo audioInfo;
}
