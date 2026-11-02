package com.moyz.adi.common.filter;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.util.JsonUtil;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;

import java.text.MessageFormat;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 鉴权白名单与管理端限制按应用内路径（请求 URI 去掉容器上下文路径）判定，根部署与子路径部署结论一致；
 * 外部 API 请求只凭 ExtApiAuthFilter 留下的上下文标记放行，不按路径放行。
 */
class TokenFilterTest {

    private TokenFilter filter;
    private ValueOperations<String, String> values;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setup() {
        filter = new TokenFilter();
        StringRedisTemplate redis = mock(StringRedisTemplate.class);
        values = mock(ValueOperations.class);
        when(redis.opsForValue()).thenReturn(values);
        ReflectionTestUtils.setField(filter, "stringRedisTemplate", redis);
    }

    @AfterEach
    void cleanup() {
        ThreadContext.unload();
    }

    @Test
    void extApiPathIsNotPassedByPathAlone() throws Exception {
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/character/chat", null));
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/character/chat", "ext-some-key"));
    }

    @Test
    void extApiRequestPassesOnUpstreamContextAndKeepsIt() throws Exception {
        User owner = new User();
        owner.setId(7L);
        ThreadContext.setCurrentUser(owner);
        ThreadContext.setExtApiContext(true, "entity-uuid", "character");
        assertEquals(HttpServletResponse.SC_OK, run("", "/ext/v1/character/chat", "ext-some-key"));
        assertTrue(ThreadContext.isExtApiRequest());
        assertEquals(7L, ThreadContext.getCurrentUser().getId());
    }

    @Test
    void extApiFilterIsOrderedBeforeTokenFilter() {
        assertTrue(ExtApiAuthFilter.class.getAnnotation(Order.class).value()
                < TokenFilter.class.getAnnotation(Order.class).value());
    }

    @Test
    void whitelistedPathPassesUnderRootAndSubContext() throws Exception {
        assertEquals(HttpServletResponse.SC_OK, run("", "/auth/login", null));
        assertEquals(HttpServletResponse.SC_OK, run("/api", "/api/auth/login", null));
        assertEquals(HttpServletResponse.SC_OK, run("/api", "/api/assets/app.js", null));
    }

    @Test
    void protectedPathWithoutTokenIsRejected() throws Exception {
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/user/info", null));
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("/api", "/api/user/info", null));
    }

    @Test
    void adminPathRequiresAdminUnderSubContext() throws Exception {
        stubUser("member-token", false);
        stubUser("admin-token", true);
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("/api", "/api/admin/user/list", "member-token"));
        assertEquals(HttpServletResponse.SC_OK, run("/api", "/api/admin/user/list", "admin-token"));
        assertEquals(HttpServletResponse.SC_OK, run("/api", "/api/user/info", "member-token"));
    }

    private void stubUser(String token, boolean admin) {
        User user = new User();
        user.setId(1L);
        user.setIsAdmin(admin);
        when(values.get(MessageFormat.format(RedisKeyConstant.USER_TOKEN, token))).thenReturn(JsonUtil.toJson(user));
    }

    private int run(String contextPath, String uri, String token) throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest("GET", uri);
        request.setContextPath(contextPath);
        if (token != null) {
            request.addHeader("Authorization", token);
        }
        MockHttpServletResponse response = new MockHttpServletResponse();
        filter.doFilter(request, response, new MockFilterChain());
        return response.getStatus();
    }
}
