package com.moyz.adi.common.aop;

import com.moyz.adi.common.annotation.DistributeLock;
import com.moyz.adi.common.util.RedisTemplateUtil;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;


/**
 * 通用分布式锁
 * Generic distributed lock aspect
 *
 * @author moyz
 */
@Slf4j
@Aspect
@Component
public class DistributeLockAspect {

    private final RedisTemplateUtil redisTemplateUtil;

    public DistributeLockAspect(RedisTemplateUtil redisTemplateUtil) {
        this.redisTemplateUtil = redisTemplateUtil;
    }

    @Around("@annotation(distributeLock)")
    public Object around(ProceedingJoinPoint joinPoint, DistributeLock distributeLock) throws Throwable {
        String key = distributeLock.redisKey();
        int expireInSeconds = distributeLock.expireInSeconds();
        boolean continueIfAcquireFail = distributeLock.continueIfAcquireFail();

        String clientId = distributeLock.clientId();
        if (StringUtils.isBlank(clientId)) {
            clientId = UuidUtil.createShort();
        }

        boolean lockAndContinue = checkAndLock(key, clientId, expireInSeconds, continueIfAcquireFail);
        if (!lockAndContinue) {
            log.warn("This request is ignored");
            return false;
        }
        try {
            return joinPoint.proceed();
        } finally {
            boolean unlockResult = redisTemplateUtil.unlock(key, clientId);
            log.info("unlock:{},key:{},clientId:{}", unlockResult, key, clientId);
        }
    }

    /**
     * 校验参数及加锁
     * Validate parameters and acquire lock
     *
     * @param key                   redis key
     * @param clientId              加锁方标识 / Lock holder identifier
     * @param expireInSeconds       超时时间 （秒） / Timeout in seconds
     * @param continueIfAcquireFail 获取锁失败是否继续执行后面的业务逻辑 / Whether to continue if lock acquisition fails
     * @return 是否成功加锁 / Whether the lock was successfully acquired
     */
    private boolean checkAndLock(String key, String clientId, int expireInSeconds, boolean continueIfAcquireFail) {
        log.info("lock info,key:{},clientId:{},expireInSecond:{},continueIfAcquireFail:{}", key, clientId, expireInSeconds, continueIfAcquireFail);
        if (StringUtils.isBlank(key) || expireInSeconds < 1) {
            log.warn("Invalid lock parameters, please check and try again");
            throw new IllegalArgumentException("Invalid lock parameters, please check and try again");
        }
        boolean lock = redisTemplateUtil.lock(key, clientId, expireInSeconds);
        if (!lock && !continueIfAcquireFail) {
            log.warn("Parameter continueIfAcquireFail is false and lock acquisition failed, this request is ignored");
            return false;
        }
        return lock;
    }
}
