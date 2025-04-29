package com.moyz.adi.common.workflow.node.mailsender;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.MAIL_SENDER_TYPE_CUSTOM;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.MAIL_SENDER_TYPE_SYS;

@Data
@Slf4j
public class MailSendNodeConfig {
    // 邮件发送人类型,1: 系统, 2: 自定义
    @Min(MAIL_SENDER_TYPE_SYS)
    @Max(MAIL_SENDER_TYPE_CUSTOM)
    @JsonProperty("sender_type")
    private int senderType;
    //多邮箱则以逗号分隔
    @JsonProperty("cc_mails")
    private String ccMails;
    //多邮箱则以逗号分隔
    @NotBlank
    @JsonProperty("to_mails")
    private String toMails;
    @NotBlank
    private String subject;
    @NotBlank
    private String content;
    private SenderInfo sender;
    private SmtpInfo smtp;

    /**
     * 自定义发送人信息
     */
    @Data
    public static class SenderInfo {
        private String name;
        private String password;
        private String mail;
    }

    @Data
    public static class SmtpInfo {
        private String host;
        private int port;
    }
}
