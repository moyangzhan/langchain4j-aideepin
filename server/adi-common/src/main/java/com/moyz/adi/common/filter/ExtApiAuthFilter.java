package com.moyz.adi.common.filter;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ExtApiResourceType;
import com.moyz.adi.common.service.ExtApiService;
import jakarta.annotation.Resource;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;

/**
 * 必须先于 {@link TokenFilter} 执行：外部 API 请求在此校验 key 并标记上下文，TokenFilter 仅凭该标记放行。
 * Must run before {@link TokenFilter}: external API keys are validated here and the context flag is the only thing
 * TokenFilter accepts for these requests.
 */
@Slf4j
@Component
@Order(Ordered.LOWEST_PRECEDENCE - 2)
public class ExtApiAuthFilter extends OncePerRequestFilter {

    private static final String API_V1_PREFIX = "/ext/v1/";
    private static final String KEY_PREFIX = "ext-";

    @Resource
    private ExtApiService extApiService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        String requestUri = request.getRequestURI();
        String path = requestUri.substring(request.getContextPath().length());

        if (!path.startsWith(API_V1_PREFIX)) {
            filterChain.doFilter(request, response);
            return;
        }

        // --- External API request handling ---
        String rawKey = request.getHeader(AUTHORIZATION);
        if (StringUtils.isBlank(rawKey) || !rawKey.startsWith(KEY_PREFIX)) {
            log.warn("ExternalAPI: missing or invalid API key, uri:{}", requestUri);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }
        rawKey = rawKey.trim();

        String type = inferType(path.substring(API_V1_PREFIX.length()));
        if (StringUtils.isBlank(type)) {
            log.warn("ExternalAPI: unable to infer resource type from uri:{}", requestUri);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }

        ExtApiService.ValidateResult result;
        try {
            result = validateByType(rawKey, type);
        } catch (Exception e) {
            log.warn("ExternalAPI: API key validation failed, uri:{}, error:{}", requestUri, e.getMessage());
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }

        User ownerUser = result.ownerUser();
        ThreadContext.setCurrentUser(ownerUser);
        ThreadContext.setExtApiContext(true, result.entityUuid(), type);

        try {
            filterChain.doFilter(request, response);
        } finally {
            ThreadContext.unload();
        }
    }

    /**
     * 根据资源类型选择验证方式
     * draw/mcp 为用户级，使用 userExtApiKey 表验证
     * character/knowledge/workflow 为资源级，使用对应资源表验证
     */
    private ExtApiService.ValidateResult validateByType(String rawKey, String type) {
        ExtApiResourceType resourceType = ExtApiResourceType.fromValue(type);
        if (resourceType != null && resourceType.isUserLevel()) {
            return extApiService.validateUserApiKey(rawKey, type);
        }
        return extApiService.validateApiKey(rawKey, type);
    }

    private String inferType(String pathAfterPrefix) {
        String firstSegment = pathAfterPrefix.split("/")[0];
        ExtApiResourceType type = switch (firstSegment) {
            case "character" -> ExtApiResourceType.CHARACTER;
            case "knowledge" -> ExtApiResourceType.KNOWLEDGE;
            case "workflow" -> ExtApiResourceType.WORKFLOW;
            case "draw" -> ExtApiResourceType.DRAW;
            case "mcp" -> ExtApiResourceType.MCP;
            default -> null;
        };
        return null != type ? type.getValue() : null;
    }
}
