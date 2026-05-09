package com.moyz.adi.common.workflow;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.TimeUnit;

/**
 * 已中断正在等待用户输入的流程 <br/>
 * TODO 需要考虑项目多节点部署的情况
 */
@Slf4j
public class InterruptedFlow {

    private InterruptedFlow() {
    }

    private static final Cache<String, WorkflowEngine> CACHE = CacheBuilder.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .maximumSize(1000)
            .removalListener(notification -> {
                log.info("InterruptedFlow移除条目,key:{},cause:{}", notification.getKey(), notification.getCause());
            })
            .build();

    public static void put(String runtimeUuid, WorkflowEngine engine) {
        CACHE.put(runtimeUuid, engine);
    }

    public static WorkflowEngine get(String runtimeUuid) {
        return CACHE.getIfPresent(runtimeUuid);
    }

    public static void remove(String runtimeUuid) {
        CACHE.invalidate(runtimeUuid);
    }

    public static void cleanUp() {
        CACHE.cleanUp();
    }

    public static long size() {
        return CACHE.size();
    }
}
