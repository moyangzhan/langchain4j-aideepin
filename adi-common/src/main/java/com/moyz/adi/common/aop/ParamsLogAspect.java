package com.moyz.adi.common.aop;

import com.moyz.adi.common.annotation.ParamsLog;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

/**
 * @author myz
 */
@Slf4j
@Aspect
@Component
public class ParamsLogAspect {

    @Before(value = "@annotation(paramsLog)")
    public void before(JoinPoint joinPoint, ParamsLog paramsLog) {
        paramsLog(joinPoint, log);
    }

    /**
     * 输出方法参数到日志
     *
     * @param joinPoint joinPoint
     * @param logger    日志
     */
    static void paramsLog(JoinPoint joinPoint, Logger logger) {
        String className = joinPoint.getSignature().getDeclaringType().getName();
        Method method = ((MethodSignature) joinPoint.getSignature()).getMethod();
        Object[] args = joinPoint.getArgs();
        Parameter[] parameters = ((MethodSignature) joinPoint.getSignature()).getMethod().getParameters();
        StringBuilder sb = new StringBuilder();
        sb.append(className);
        sb.append(".");
        sb.append(method.getName());
        sb.append(" params:[");
        for (int i = 0; i < args.length; i++) {
            String paramName = parameters[i].getName();
            sb.append(parameters[i].getName());
            sb.append("=>");
            if ("password".equals(paramName)) {
                sb.append("***");
            } else {
                sb.append(args[i]);
            }
            sb.append(";");
        }
        sb.append("]");
        String log = sb.toString();
        logger.info(StringUtils.substring(log, 0, 1000));
    }
}
