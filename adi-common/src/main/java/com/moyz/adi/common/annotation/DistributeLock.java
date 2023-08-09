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

}
