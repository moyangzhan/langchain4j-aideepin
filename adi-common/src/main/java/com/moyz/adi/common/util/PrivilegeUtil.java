package com.moyz.adi.common.util;

import com.baomidou.mybatisplus.extension.conditions.query.QueryChainWrapper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;

public class PrivilegeUtil {

    private static final String DELETE_COLUMN_NAME = "is_deleted";

    private PrivilegeUtil() {
    }

    public static <T> T checkAndGetById(Long id, QueryChainWrapper<T> lambdaQueryChainWrapper, ErrorEnum exceptionMessage) {
        return checkAndGet(id, null, lambdaQueryChainWrapper, exceptionMessage);
    }

    public static <T> T checkAndGetByUuid(String uuid, QueryChainWrapper<T> lambdaQueryChainWrapper, ErrorEnum exceptionMessage) {
        return checkAndGet(null, uuid, lambdaQueryChainWrapper, exceptionMessage);
    }

    public static <T> T checkAndGet(Long id, String uuid, QueryChainWrapper<T> lambdaQueryChainWrapper, ErrorEnum exceptionMessage) {
        T target;
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            target = lambdaQueryChainWrapper
                    .eq(null != id, "id", id)
                    .eq(null != uuid, "uuid", uuid)
                    .eq(DELETE_COLUMN_NAME, false).oneOpt()
                    .orElse(null);
        } else {
            target = lambdaQueryChainWrapper
                    .eq(null != id, "id", id)
                    .eq(null != uuid, "uuid", uuid)
                    .eq("user_id", ThreadContext.getCurrentUserId())
                    .eq(DELETE_COLUMN_NAME, false)
                    .oneOpt()
                    .orElse(null);
        }
        if (null == target) {
            throw new BaseException(exceptionMessage);
        }
        return target;
    }
}
