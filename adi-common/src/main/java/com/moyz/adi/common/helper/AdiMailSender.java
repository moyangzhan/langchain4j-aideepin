package com.moyz.adi.common.helper;

import com.moyz.adi.common.vo.CustomMailInfo;
import jakarta.annotation.Resource;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.util.Properties;

@Slf4j
@Service
public class AdiMailSender {

    @Resource
    private JavaMailSender javaMailSender;

    @Value("${spring.application.name}")
    private String appName;

    @Value("${spring.mail.username}")
    private String senderMail;

    public void send(String subject, String content, String to) {
        this.send(subject, content, to, null);
    }

    public void send(String subject, String content, String to, String cc) {
        log.info("mail sender:{}", senderMail);
        if (StringUtils.isAnyBlank(senderMail, to)) {
            return;
        }
        CustomMailInfo customMailInfo = new CustomMailInfo();
        customMailInfo.setSenderMail(senderMail);
        customMailInfo.setSenderName(appName);
        customMailInfo.setCcMails(cc);
        customMailInfo.setToMails(to);
        customMailInfo.setSubject(subject);
        customMailInfo.setContent(content);
        sendBySender(javaMailSender, customMailInfo);
    }

    public void customSend(CustomMailInfo customMailInfo) {
        JavaMailSenderImpl mailSender = new JavaMailSenderImpl();
        mailSender.setHost(customMailInfo.getHost());
        mailSender.setPort(customMailInfo.getPort());
        mailSender.setUsername(customMailInfo.getSenderMail());
        mailSender.setPassword(customMailInfo.getSenderPassword());
        mailSender.setDefaultEncoding(StandardCharsets.UTF_8.name());
        mailSender.setProtocol("smtps");
        Properties props = mailSender.getJavaMailProperties();
        props.put("mail.smtp.ssl.enable", "true");
        sendBySender(mailSender, customMailInfo);
    }

    private void sendBySender(JavaMailSender mailSender, CustomMailInfo customMailInfo) {
        MimeMessage message = mailSender.createMimeMessage();
        try {
            MimeMessageHelper helper = new MimeMessageHelper(message, true);
            InternetAddress fromAddress = new InternetAddress(customMailInfo.getSenderMail(), customMailInfo.getSenderName());
            helper.setFrom(fromAddress);
            if (StringUtils.isNotBlank(customMailInfo.getCcMails())) {
                helper.setCc(customMailInfo.getCcMails());
            }
            helper.setTo(customMailInfo.getToMails());
            helper.setSubject(customMailInfo.getSubject());
            helper.setText(customMailInfo.getContent());
            mailSender.send(message);
        } catch (Exception e) {
            log.error("发送邮件时发生异常", e);
        }
    }
}
