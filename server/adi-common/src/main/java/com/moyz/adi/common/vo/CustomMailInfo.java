package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class CustomMailInfo {
    private String host;
    private Integer port;
    private String senderName;
    private String senderMail;
    private String senderPassword;
    private String ccMails;
    private String toMails;
    private String subject;
    private String content;
}
