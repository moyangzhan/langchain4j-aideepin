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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;

@Slf4j
@Component
public class ExtApiAuthFilter extends OncePerRequestFilter {

    private static final String API_V1_PREFIX = "/api/v1/";
    private static final String KEY_PREFIX = "adi-";

    @Value("${server.servlet.context-path:}")
    private String contextPath;

    @Resource
    private ExtApiService extApiService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        String requestUri = request.getRequestURI();
        String apiPath = contextPath + API_V1_PREFIX;

        if (!requestUri.startsWith(apiPath)) {
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

        String type = inferType(requestUri);
        if (StringUtils.isBlank(type)) {
            log.warn("ExternalAPI: unable to infer resource type from uri:{}", requestUri);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }

        ExtApiService.ValidateResult result;
        try {
            result = extApiService.validateApiKey(rawKey, type);
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

    private String inferType(String requestUri) {
        String apiPath = contextPath + API_V1_PREFIX;
        String pathAfterPrefix = requestUri.substring(apiPath.length());
        String firstSegment = pathAfterPrefix.split("/")[0];
        ExtApiResourceType type = switch (firstSegment) {
            case "character" -> ExtApiResourceType.CHARACTER;
            case "knowledge" -> ExtApiResourceType.KNOWLEDGE;
            case "workflow" -> ExtApiResourceType.WORKFLOW;
            default -> null;
        };
        return null != type ? type.getValue() : null;
    }
}
