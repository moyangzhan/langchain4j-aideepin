package com.moyz.adi.common.util;

import jakarta.annotation.Resource;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

@Component
public class RedisTemplateUtil {

    private static final String UNLOCK_SCRIPT =
            "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) else return 0 end";

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    public boolean lock(String key, String clientId, int lockExpireInSecond) {
        return Boolean.TRUE.equals(stringRedisTemplate.opsForValue().setIfAbsent(key, clientId, lockExpireInSecond, TimeUnit.SECONDS));
    }

    public boolean unlock(String key, String clientId) {
        RedisScript<Long> redisScript = RedisScript.of(UNLOCK_SCRIPT, Long.class);
        Long result = stringRedisTemplate.execute(redisScript, Collections.singletonList(key), clientId);
        return result != null && result > 0;
    }

}
