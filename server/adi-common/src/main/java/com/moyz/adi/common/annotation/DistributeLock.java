package com.moyz.adi.common.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 分布式锁注解
 *
 * @author moyz
 * date:2021-07-15
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface DistributeLock {

    /**
     * redis key
     *
     * @return
     */
    String redisKey() default "";

    /**
     * clientId标识用来加锁的客户端
     *
     * @return
     */
    String clientId() default "";

    /**
     * 失效时间（秒）
     *
     * @return
     */
    int expireInSeconds() default 0;

    /**
     * 如果获取锁失败，是否继续执行
     *
     * @return
     */
    boolean continueIfAcquireFail() default true;

    /**
     * 为 true 时即使执行成功也不主动释放锁，仅由 TTL 到期自动释放，用于拉长同名任务在多实例下的集群最小执行间隔。
     * When true the aspect never unlocks even on success; the lock is released by TTL alone,
     * pacing same-name jobs across instances.
     */
    boolean unlockByTimeout() default false;

}
