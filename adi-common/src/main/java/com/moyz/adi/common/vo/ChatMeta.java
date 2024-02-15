package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ChatMeta {
    private QuestionMeta question;
    private AnswerMeta answer;
}
