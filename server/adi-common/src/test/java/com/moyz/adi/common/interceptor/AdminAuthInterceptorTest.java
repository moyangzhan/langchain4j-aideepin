package com.moyz.adi.common.interceptor;

import com.moyz.adi.admin.controller.AdminPackageController;
import com.moyz.adi.adminx.AdminxPackageController;
import com.moyz.adi.chat.ChatPackageController;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.User;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.method.HandlerMethod;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * 管理端角色闸按处理器所在包判定：admin 包内非管理员/无用户一律 401，
 * admin 包之外的处理器与非 HandlerMethod 请求处理器直通。
 */
class AdminAuthInterceptorTest {

    private final AdminAuthInterceptor interceptor = new AdminAuthInterceptor();

    @AfterEach
    void cleanup() {
        ThreadContext.unload();
    }

    @Test
    void adminHandlerAllowsAdminUser() throws Exception {
        ThreadContext.setCurrentUser(user(true));

        assertTrue(preHandleAdminPath(adminHandler()));
    }

    @Test
    void adminHandlerRejectsNonAdminUser() throws Exception {
        ThreadContext.setCurrentUser(user(false));
        MockHttpServletResponse response = new MockHttpServletResponse();

        boolean allowed = interceptor.preHandle(request(), response, adminHandler());

        assertFalse(allowed);
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, response.getStatus());
    }

    @Test
    void adminHandlerRejectsMissingUser() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();

        boolean allowed = interceptor.preHandle(request(), response, adminHandler());

        assertFalse(allowed);
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, response.getStatus());
    }

    @Test
    void chatPackageHandlerPassesNonAdminUser() throws Exception {
        ThreadContext.setCurrentUser(user(false));

        assertTrue(preHandleAdminPath(handler(ChatPackageController.class)));
    }

    @Test
    void chatPackageHandlerPassesWithoutUser() throws Exception {
        assertTrue(preHandleAdminPath(handler(ChatPackageController.class)));
    }

    @Test
    void adminxPackageStaysBehindTheGate() throws Exception {
        ThreadContext.setCurrentUser(user(false));
        MockHttpServletResponse response = new MockHttpServletResponse();

        boolean allowed = interceptor.preHandle(request(), response, handler(AdminxPackageController.class));

        assertFalse(allowed);
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, response.getStatus());
    }

    @Test
    void adminxPackageAllowsAdminUser() throws Exception {
        ThreadContext.setCurrentUser(user(true));

        assertTrue(preHandleAdminPath(handler(AdminxPackageController.class)));
    }

    @Test
    void nonHandlerMethodPassesWithoutUser() {
        MockHttpServletResponse response = new MockHttpServletResponse();

        boolean allowed = interceptor.preHandle(request(), response, new Object());

        assertTrue(allowed);
        assertEquals(HttpServletResponse.SC_OK, response.getStatus());
    }

    private boolean preHandleAdminPath(HandlerMethod handler) throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        boolean allowed = interceptor.preHandle(request(), response, handler);
        assertEquals(HttpServletResponse.SC_OK, response.getStatus());
        return allowed;
    }

    private static MockHttpServletRequest request() {
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setRequestURI("/admin/user/list");
        return request;
    }

    private static HandlerMethod adminHandler() throws Exception {
        return handler(AdminPackageController.class);
    }

    private static HandlerMethod handler(Class<?> controllerType) throws Exception {
        return new HandlerMethod(controllerType.getDeclaredConstructor().newInstance(),
                controllerType.getDeclaredMethod("endpoint"));
    }

    private static User user(boolean isAdmin) {
        User user = new User();
        user.setId(7L);
        user.setIsAdmin(isAdmin);
        return user;
    }
}
