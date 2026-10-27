package com.moyz.adi.common.aop;

import com.moyz.adi.common.annotation.DistributeLock;
import com.moyz.adi.common.util.RedisTemplateUtil;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
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
            return lockFailReturnValue(joinPoint);
        }
        try {
            return joinPoint.proceed();
        } finally {
            boolean unlockResult = redisTemplateUtil.unlock(key, clientId);
            log.info("unlock:{},key:{},clientId:{}", unlockResult, key, clientId);
        }
    }

    /**
     * 锁失败时的占位返回值，按目标方法返回类型匹配：void/Void 与对象类型返回 null，boolean 返回 false；
     * 其余原始类型无法用 null 占位（调用方拆箱即 NPE），明确报错。
     * <p>
     * Lock-fail placeholder matching the target's return type so the aspect is safe on void methods (scheduled jobs):
     * void/Void and object types get null, boolean gets false.
     * Other primitives cannot be null — returning null would surface as an unboxing NPE at the caller, so fail fast with a clear message instead.
     * </p>
     */
    private Object lockFailReturnValue(ProceedingJoinPoint joinPoint) {
        if (joinPoint.getSignature() instanceof MethodSignature signature) {
            Class<?> returnType = signature.getMethod().getReturnType();
            if (boolean.class == returnType) {
                return false;
            }
            if (returnType.isPrimitive()) {
                throw new UnsupportedOperationException(
                        "@DistributeLock does not support primitive return types other than boolean: " + returnType.getName());
            }
        }
        return null;
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
