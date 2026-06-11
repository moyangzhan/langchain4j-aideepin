package com.moyz.adi.common.base;

import com.moyz.adi.common.service.KnowledgeBaseService;
import com.moyz.adi.common.service.ModelHealthService;
import com.moyz.adi.common.service.SysConfigService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class Jobs {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private ModelHealthService modelHealthService;

    @Scheduled(initialDelay = 180_000, fixedDelay = 10 * 60_000)
    public void scheduledHealthCheck() {
        log.info("Scheduled health check starting");
        modelHealthService.checkAll();
    }

    @Scheduled(initialDelay = 10 * 60 * 1000, fixedDelay = 10 * 60 * 1000)
    public void reloadConfig() {
        sysConfigService.loadAndCache();
    }

    @Scheduled(fixedDelay = 60 * 1000)
    public void run() {
        knowledgeBaseService.updateStatistic();
    }
}
