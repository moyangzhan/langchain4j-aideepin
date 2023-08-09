package com.moyz.adi.common.base;

import com.moyz.adi.common.entity.User;

public class ThreadContext {
    private static final ThreadLocal<User> currentUser = new ThreadLocal<>();
    private static final ThreadLocal<String> currentToken = new ThreadLocal<>();

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


    public static String getToken() {
        return currentToken.get();
    }


    public static User getExistCurrentUser() {
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new RuntimeException("用户不存在");
        }
        return user;
    }
}
