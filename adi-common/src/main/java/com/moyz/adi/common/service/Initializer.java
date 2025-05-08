package com.moyz.adi.common.service;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.file.AliyunOssFileHelper;
import com.moyz.adi.common.file.AliyunOssFileOperator;
import com.moyz.adi.common.file.LocalFileOperator;
import com.moyz.adi.common.util.AesUtil;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class Initializer {

    @Value("${local.images}")
    private String imagePath;

    @Value("${local.tmp-images}")
    private String tmpImagePath;

    @Value("${local.thumbnails}")
    private String thumbnailsPath;

    @Value("${local.files}")
    private String filePath;

    @Value("${local.chat-memory}")
    private String chatMemoryPath;

    @Resource
    private AdiProperties adiProperties;

    @Resource
    private AiModelService aiModelService;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private AliyunOssFileHelper aliyunOssFileHelper;

    /**
     * 应用初始化
     */
    @PostConstruct
    public void init() {
        if (adiProperties.getEncrypt().getAesKey().equals("Ap9da0CopbjiKGc1")) {
            throw new RuntimeException("不能使用默认的AES key，请设置属于你自己的Key，AES相关的加解密都会用到该key，设置路径: application.yml => adi.encrypt.aes-key");
        }
        sysConfigService.loadAndCache();
        aiModelService.init();
        checkAndInitFileOperator();

        AesUtil.AES_KEY = adiProperties.getEncrypt().getAesKey();
    }

    /**
     * 10分钟重刷一次配置信息
     */
    @Scheduled(initialDelay = 10 * 60 * 1000, fixedDelay = 10 * 60 * 1000)
    public void reloadConfig() {
        sysConfigService.loadAndCache();
    }

    public void checkAndInitFileOperator() {
        log.info("Initializing file operator...");
        LocalFileOperator.checkAndCreateDir(imagePath);
        LocalFileOperator.checkAndCreateDir(tmpImagePath);
        LocalFileOperator.checkAndCreateDir(thumbnailsPath);
        LocalFileOperator.checkAndCreateDir(filePath);
        LocalFileOperator.checkAndCreateDir(chatMemoryPath);
        LocalFileOperator.init(imagePath, filePath);
        AliyunOssFileOperator.init(aliyunOssFileHelper);
    }
}
