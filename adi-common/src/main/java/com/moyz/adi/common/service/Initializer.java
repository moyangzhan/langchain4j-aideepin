package com.moyz.adi.common.service;

import com.moyz.adi.common.helper.EmbeddingHelper;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;

@Service
public class Initializer {

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private EmbeddingHelper embeddingHelper;

    @PostConstruct
    public void init(){
        sysConfigService.reload();
        embeddingHelper.init();
    }
}
