package com.moyz.adi.common.base;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import io.micrometer.common.util.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.A_USER_NOT_FOUND;

public class ThreadContext {
    private static final ThreadLocal<User> currentUser = new ThreadLocal<>();
    private static final ThreadLocal<String> currentToken = new ThreadLocal<>();
    private static final ThreadLocal<Boolean> extApiRequest = new ThreadLocal<>();
    private static final ThreadLocal<String> extApiEntityUuid = new ThreadLocal<>();
    private static final ThreadLocal<String> extApiEntityType = new ThreadLocal<>();

    public static void setCurrentUser(User user) {
        currentUser.set(user);
    }

    public static User getCurrentUser() {
        return currentUser.get();
    }

    public static Long getCurrentUserId() {
        return currentUser.get().getId();
    }

    public static void setToken(String token) {
        currentToken.set(token);
    }

    private ThreadContext(){}

    public static String getToken() {
        return currentToken.get();
    }

    public static boolean isLogin(){
        return StringUtils.isNotBlank(currentToken.get());
    }

    public static void setExtApiContext(boolean isRequest, String entityUuid, String entityType) {
        extApiRequest.set(isRequest);
        extApiEntityUuid.set(entityUuid);
        extApiEntityType.set(entityType);
    }

    public static boolean isExtApiRequest() {
        return Boolean.TRUE.equals(extApiRequest.get());
    }

    public static String getExtApiEntityUuid() {
        return extApiEntityUuid.get();
    }

    public static String getExtApiEntityType() {
        return extApiEntityType.get();
    }

    public static void unload() {
        currentUser.remove();
        currentToken.remove();
        extApiRequest.remove();
        extApiEntityUuid.remove();
        extApiEntityType.remove();
    }

    public static User getExistCurrentUser() {
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_FOUND);
        }
        return user;
    }
}
