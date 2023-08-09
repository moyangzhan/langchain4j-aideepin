package com.moyz.adi.common.helper;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class AdiMailSender {

    @Resource
    private JavaMailSender javaMailSender;

    @Value("${spring.mail.username}")
    private String from;

    public void send(String subject, String content, String to) {
        log.info("mail sender:{}", from);
        if (StringUtils.isAnyBlank(from, to)) {
            return;
        }
        MimeMessage message = javaMailSender.createMimeMessage();
        try {
            MimeMessageHelper helper = new MimeMessageHelper(message, true);
            // 设置发件人名称和地址
            InternetAddress fromAddress = new InternetAddress(from, "AIDeepIn");
            helper.setFrom(fromAddress);

            // 设置收件人、主题、内容等其他信息
            helper.setTo(to);
            helper.setSubject(subject);
            helper.setText(content);

            javaMailSender.send(message);
        } catch (Exception e) {
            log.error("发送邮件时发生异常", e);
        }
//        SimpleMailMessage message = new SimpleMailMessage();
//        message.setFrom(from);
//        message.setTo(to);
//        message.setSubject(subject);
//        message.setText(content);
//        try {
//            javaMailSender.send(message);
//        } catch (Exception e) {
//            log.error("发送邮件时发生异常", e);
//        }
    }
}
