package com.moyz.adi.common.aop;

import com.moyz.adi.common.annotation.DistributeLock;
import com.moyz.adi.common.util.RedisTemplateUtil;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Acquire/release contract of the lock aspect: on acquisition failure the method is skipped with a
 * placeholder return unless continueIfAcquireFail=true, in which case it runs without the lock and the
 * finally block releases nothing. When the lock was acquired it is actively unlocked in finally on both
 * success and failure; with unlockByTimeout=true the aspect never unlocks — success and exception alike —
 * leaving the release to the key TTL, so a same-name scheduled job runs at most once per TTL window
 * across instances.
 * <p>
 * 锁切面的获取/释放契约：获取失败时跳过方法并返回占位值，除非 continueIfAcquireFail=true——此时无锁执行
 * 且 finally 不做任何释放。获取成功时默认在 finally 中无论成功失败都主动释放；unlockByTimeout=true 时
 * 切面永不主动释放（成功与异常皆然），交由 key 的 TTL 到期释放，使同名定时任务在多实例下每个 TTL 窗口
 * 至多执行一次。
 * </p>
 */
class DistributeLockAspectTest {

    static class Fixture {

        @DistributeLock(redisKey = "lock:test", expireInSeconds = 60, continueIfAcquireFail = false, unlockByTimeout = true)
        public void held() {
        }

        @DistributeLock(redisKey = "lock:test", expireInSeconds = 60, continueIfAcquireFail = false)
        public void released() {
        }

        @DistributeLock(redisKey = "lock:test", expireInSeconds = 60, continueIfAcquireFail = true)
        public void continueOnFail() {
        }

        @DistributeLock(redisKey = "lock:test", expireInSeconds = 60, continueIfAcquireFail = false)
        public boolean bool() {
            return true;
        }

        @DistributeLock(redisKey = "lock:test", expireInSeconds = 60, continueIfAcquireFail = false)
        public int count() {
            return 42;
        }
    }

    private DistributeLockAspect aspect;
    private RedisTemplateUtil redisTemplateUtil;
    private ProceedingJoinPoint joinPoint;

    @BeforeEach
    void setUp() {
        redisTemplateUtil = mock(RedisTemplateUtil.class);
        joinPoint = mock(ProceedingJoinPoint.class);
        aspect = new DistributeLockAspect(redisTemplateUtil);
        when(redisTemplateUtil.lock(anyString(), anyString(), eq(60L))).thenReturn(true);
    }

    @Test
    void unlockByTimeoutNeverUnlocksAfterSuccess() throws Throwable {
        when(joinPoint.proceed()).thenReturn(null);

        aspect.around(joinPoint, annotation("held"));

        verify(joinPoint).proceed();
        verify(redisTemplateUtil).lock(eq("lock:test"), anyString(), eq(60L));
        verify(redisTemplateUtil, never()).unlock(anyString(), anyString());
    }

    @Test
    void unlockByTimeoutNeverUnlocksAfterFailure() throws Throwable {
        when(joinPoint.proceed()).thenThrow(new IllegalStateException("boom"));

        assertThrows(IllegalStateException.class, () -> aspect.around(joinPoint, annotation("held")));

        verify(joinPoint).proceed();
        verify(redisTemplateUtil, never()).unlock(anyString(), anyString());
    }

    @Test
    void defaultUnlocksAfterSuccess() throws Throwable {
        when(joinPoint.proceed()).thenReturn(null);

        aspect.around(joinPoint, annotation("released"));

        verify(joinPoint).proceed();
        verify(redisTemplateUtil).unlock(eq("lock:test"), anyString());
    }

    @Test
    void defaultUnlocksAfterFailure() throws Throwable {
        when(joinPoint.proceed()).thenThrow(new IllegalStateException("boom"));

        assertThrows(IllegalStateException.class, () -> aspect.around(joinPoint, annotation("released")));

        verify(joinPoint).proceed();
        verify(redisTemplateUtil).unlock(eq("lock:test"), anyString());
    }

    @Test
    void lockFailWithContinueFlagProceedsWithoutLock() throws Throwable {
        when(redisTemplateUtil.lock(anyString(), anyString(), eq(60L))).thenReturn(false);
        when(joinPoint.proceed()).thenReturn("done");

        Object result = aspect.around(joinPoint, annotation("continueOnFail"));

        assertEquals("done", result);
        verify(joinPoint).proceed();
        verify(redisTemplateUtil, never()).unlock(anyString(), anyString());
    }

    @Test
    void lockFailWithoutContinueFlagSkipsMethod() throws Throwable {
        when(redisTemplateUtil.lock(anyString(), anyString(), eq(60L))).thenReturn(false);
        stubSignature("released");

        Object result = aspect.around(joinPoint, annotation("released"));

        assertNull(result);
        verify(joinPoint, never()).proceed();
        verify(redisTemplateUtil, never()).unlock(anyString(), anyString());
    }

    @Test
    void lockFailReturnsFalseForBooleanMethod() throws Throwable {
        when(redisTemplateUtil.lock(anyString(), anyString(), eq(60L))).thenReturn(false);
        stubSignature("bool");

        Object result = aspect.around(joinPoint, annotation("bool"));

        assertEquals(false, result);
        verify(joinPoint, never()).proceed();
    }

    @Test
    void lockFailRejectsOtherPrimitiveReturnTypes() throws Throwable {
        when(redisTemplateUtil.lock(anyString(), anyString(), eq(60L))).thenReturn(false);
        stubSignature("count");

        assertThrows(UnsupportedOperationException.class, () -> aspect.around(joinPoint, annotation("count")));
        verify(joinPoint, never()).proceed();
    }

    private void stubSignature(String methodName) throws NoSuchMethodException {
        MethodSignature signature = mock(MethodSignature.class);
        when(signature.getMethod()).thenReturn(Fixture.class.getMethod(methodName));
        when(joinPoint.getSignature()).thenReturn(signature);
    }

    private DistributeLock annotation(String methodName) throws NoSuchMethodException {
        return Fixture.class.getMethod(methodName).getAnnotation(DistributeLock.class);
    }
}
