package com.moyz.adi.common.filter;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.service.ExtApiService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * 外部 API 门控按应用内路径的完整段匹配 /ext/v1，key 校验通过后把资源 owner 与 ext 上下文绑定到线程并在请求结束时清理。
 */
class ExtApiAuthFilterTest {

    private ExtApiAuthFilter filter;
    private ExtApiService extApiService;

    @BeforeEach
    void setup() {
        filter = new ExtApiAuthFilter();
        extApiService = mock(ExtApiService.class);
        ReflectionTestUtils.setField(filter, "extApiService", extApiService);
    }

    @AfterEach
    void cleanup() {
        ThreadContext.unload();
    }

    @Test
    void pathsOutsideExtPrefixPassThroughUntouched() throws Exception {
        assertEquals(HttpServletResponse.SC_OK, run("", "/auth/login", null));
        assertEquals(HttpServletResponse.SC_OK, run("", "/ext/v1", "ext-key"));
        assertEquals(HttpServletResponse.SC_OK, run("", "/ext/v1x/character/chat", "ext-key"));
        assertEquals(HttpServletResponse.SC_OK, run("", "/ext/v10/character/chat", "ext-key"));
        assertFalse(ThreadContext.isExtApiRequest());
        verifyNoInteractions(extApiService);
    }

    @Test
    void missingOrForeignKeyIsRejectedBeforeValidation() throws Exception {
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/character/chat", null));
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/character/chat", "Bearer abc"));
        verifyNoInteractions(extApiService);
    }

    @Test
    void unknownResourceTypeIsRejectedBeforeValidation() throws Exception {
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/unknown/x", "ext-key"));
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/", "ext-key"));
        verifyNoInteractions(extApiService);
    }

    @Test
    void failedValidationIsRejected() throws Exception {
        when(extApiService.validateApiKey("ext-key", "character")).thenThrow(new RuntimeException("fixture"));
        assertEquals(HttpServletResponse.SC_UNAUTHORIZED, run("", "/ext/v1/character/chat", "ext-key"));
        assertFalse(ThreadContext.isExtApiRequest());
    }

    @Test
    void validKeyBindsOwnerContextForTheRequestOnly() throws Exception {
        User owner = new User();
        owner.setId(7L);
        when(extApiService.validateApiKey("ext-key", "character")).thenReturn(new ExtApiService.ValidateResult("entity-1", owner));
        Long[] seenUser = new Long[1];
        boolean[] seenExt = new boolean[1];
        String[] seenEntity = new String[1];
        FilterChain capture = (request, response) -> {
            seenUser[0] = ThreadContext.getCurrentUserId();
            seenExt[0] = ThreadContext.isExtApiRequest();
            seenEntity[0] = ThreadContext.getExtApiEntityUuid();
        };
        assertEquals(HttpServletResponse.SC_OK, run("", "/ext/v1/character/chat", "ext-key", capture));
        assertEquals(7L, seenUser[0]);
        assertTrue(seenExt[0]);
        assertEquals("entity-1", seenEntity[0]);
        assertFalse(ThreadContext.isExtApiRequest());
        assertEquals(HttpServletResponse.SC_OK, run("/api", "/api/ext/v1/character/chat", "ext-key", capture));
        assertEquals("entity-1", seenEntity[0]);
        verify(extApiService, times(2)).validateApiKey("ext-key", "character");
    }

    private int run(String contextPath, String uri, String key) throws Exception {
        return run(contextPath, uri, key, (request, response) -> {
        });
    }

    private int run(String contextPath, String uri, String key, FilterChain chain) throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest("POST", uri);
        request.setContextPath(contextPath);
        if (key != null) {
            request.addHeader("Authorization", key);
        }
        MockHttpServletResponse response = new MockHttpServletResponse();
        filter.doFilter(request, response, chain);
        return response.getStatus();
    }
}
