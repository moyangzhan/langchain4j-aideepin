package com.moyz.adi.common.service;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.file.AliyunOssFileHelper;
import com.moyz.adi.common.file.AliyunOssFileOperator;
import com.moyz.adi.common.file.LocalFileOperator;
import com.moyz.adi.common.rag.*;
import com.moyz.adi.common.util.AesUtil;
import com.moyz.adi.common.entity.SysConfig;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.embedding.EmbeddingModel;
import dev.langchain4j.store.embedding.EmbeddingStore;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.Locale;

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

    @Lazy
    @Resource
    private GraphStore kbGraphStore;
    @Lazy
    @Resource
    private EmbeddingStore<TextSegment> kbEmbeddingStore;
    @Lazy
    @Resource
    private EmbeddingStore<TextSegment> semanticEmbeddingStore;
    @Lazy
    @Resource
    private EmbeddingStore<TextSegment> episodicEmbeddingStore;
    @Lazy
    @Resource
    private EmbeddingModel embeddingModel;

    /**
     * 应用初始化
     */
    @PostConstruct
    public void init() {
        if (adiProperties.getEncrypt().getAesKey().equals("Ap9da0CopbjiKGc1")) {
            throw new RuntimeException("Cannot use the default AES key. Please set your own key in application.yml => adi.encrypt.aes-key");
        }
        sysConfigService.loadAndCache();
        initDefaultLocale();
        aiModelService.init();
        checkAndInitFileOperator();

        AesUtil.AES_KEY = adiProperties.getEncrypt().getAesKey();

        // 使用召回内容来源(RetrieveContentFrom)做为RAG名称以区分不同RAG实例
        EmbeddingRagContext.add(new EmbeddingRag(AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE, embeddingModel, kbEmbeddingStore));
        EmbeddingRagContext.add(new EmbeddingRag(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY, embeddingModel, semanticEmbeddingStore));
        // 情景记忆走独立向量库（与语义记忆物理隔离），单独注册一个 RAG 实例供 retrieve 使用。
        // <p>
        // Episodic memory lives in its own physically isolated vector store; register a
        // dedicated RAG instance so retrieval routes to that store instead of the semantic one.
        EmbeddingRagContext.add(new EmbeddingRag(AdiConstant.RetrieveContentFrom.CHARACTER_MEMORY_EPISODIC, embeddingModel, episodicEmbeddingStore));

        GraphRagContext.add(new GraphRag(AdiConstant.RetrieveContentFrom.KNOWLEDGE_BASE, kbGraphStore));
    }

    /**
     * Initialize default locale if not exists.
     * Detect from system locale: zh-CN for Chinese, en-US otherwise.
     */
    private void initDefaultLocale() {
        String key = AdiConstant.SysConfigKey.DEFAULT_LOCALE;
        String existing = SysConfigService.getByKey(key);
        if (existing != null) {
            return;
        }
        Locale sysLocale = Locale.getDefault();
        String defaultLocale = Locale.CHINA.getLanguage().equals(sysLocale.getLanguage()) ? "zh-CN" : "en-US";
        log.info("Default locale not found in config, initializing with: {}", defaultLocale);
        SysConfig config = new SysConfig();
        config.setName(key);
        config.setValue(defaultLocale);
        sysConfigService.save(config);
        sysConfigService.loadAndCache();
    }

    public void checkAndInitFileOperator() {
        log.info("Initializing file operator...");
        LocalFileOperator.checkAndCreateDir(imagePath);
        LocalFileOperator.checkAndCreateDir(tmpImagePath);
        LocalFileOperator.checkAndCreateDir(thumbnailsPath);
        LocalFileOperator.checkAndCreateDir(filePath);
        LocalFileOperator.checkAndCreateDir(chatMemoryPath);
        LocalFileOperator.init(imagePath, tmpImagePath, filePath);
        AliyunOssFileOperator.init(aliyunOssFileHelper);
    }
}
