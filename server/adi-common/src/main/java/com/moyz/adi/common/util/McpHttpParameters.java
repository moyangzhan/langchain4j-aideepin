package com.moyz.adi.common.util;

import com.moyz.adi.common.dto.mcp.McpCommonParam;
import com.moyz.adi.common.dto.mcp.McpCustomizedParamDefinition;
import com.moyz.adi.common.dto.mcp.UserMcpCustomizedParam;
import com.moyz.adi.common.entity.Mcp;
import com.moyz.adi.common.entity.UserMcp;

import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/** Resolves each service's HTTP parameters independently, without mixing header values into its URL. */
public record McpHttpParameters(String url, Map<String, String> headers) {

    public McpHttpParameters {
        headers = Map.copyOf(headers);
    }

    public static void validate(Mcp mcp) {
        Set<String> headers = new HashSet<>();
        Map<String, Boolean> queries = new HashMap<>();
        for (McpCommonParam param : orEmpty(mcp.getPresetParams())) {
            validateBinding(binding(param.getName(), param.getBindType(), param.getBindName()),
                    param.getBindType() != null || param.getBindName() != null, headers, queries);
        }
        for (McpCustomizedParamDefinition definition : orEmpty(mcp.getCustomizedParamDefinitions())) {
            validateBinding(binding(definition.getName(), definition.getBindType(), definition.getBindName()),
                    definition.getBindType() != null || definition.getBindName() != null
                            || definition.getBindValueTemplate() != null, headers, queries);
            validateTemplate(definition.getBindValueTemplate());
        }
    }

    private static void validateBinding(Binding binding, boolean explicit, Set<String> headers,
                                        Map<String, Boolean> queries) {
        if (binding.header()) {
            if (!headers.add(binding.name().toLowerCase(Locale.ROOT))) {
                throw invalid("Duplicate HTTP header binding");
            }
        } else {
            if (queries.containsKey(binding.name()) && (explicit || queries.get(binding.name()))) {
                throw invalid("Duplicate HTTP query binding");
            }
            queries.put(binding.name(), explicit);
        }
    }

    public static McpHttpParameters resolve(Mcp mcp, UserMcp userMcp) {
        validate(mcp);
        Map<String, String> query = new LinkedHashMap<>();
        Map<String, String> headers = new LinkedHashMap<>();
        Set<String> headerNames = new HashSet<>();
        Set<String> explicitQueryNames = new HashSet<>();
        Map<String, UserMcpCustomizedParam> userValues = new HashMap<>();
        for (UserMcpCustomizedParam param : orEmpty(userMcp.getMcpCustomizedParams())) {
            if (param.getName() == null || userValues.putIfAbsent(param.getName(), param) != null) {
                throw invalid("Duplicate or missing user parameter name");
            }
        }
        for (McpCommonParam param : orEmpty(mcp.getPresetParams())) {
            Binding binding = binding(param.getName(), param.getBindType(), param.getBindName());
            add(binding, param.getValue(), null, param.getBindType() != null || param.getBindName() != null,
                    query, headers, headerNames, explicitQueryNames);
        }
        for (McpCustomizedParamDefinition definition : orEmpty(mcp.getCustomizedParamDefinitions())) {
            Binding binding = binding(definition.getName(), definition.getBindType(), definition.getBindName());
            UserMcpCustomizedParam param = userValues.get(definition.getName());
            boolean explicit = definition.getBindType() != null || definition.getBindName() != null
                    || definition.getBindValueTemplate() != null;
            add(binding, param == null ? null : param.getValue(), definition.getBindValueTemplate(), explicit,
                    query, headers, headerNames, explicitQueryNames);
        }
        return new McpHttpParameters(appendQuery(mcp.getSseUrl(), query), headers);
    }

    private static void add(Binding binding, Object rawValue, String template, boolean explicit,
                            Map<String, String> query, Map<String, String> headers,
                            Set<String> headerNames, Set<String> explicitQueryNames) {
        if (rawValue == null || rawValue instanceof String && ((String) rawValue).isBlank()) {
            if (explicit) {
                throw invalid("Missing HTTP parameter value");
            }
            return; // Legacy query parameters remain optional; null is never converted to the literal "null".
        }
        if (!(rawValue instanceof String || rawValue instanceof Number || rawValue instanceof Boolean)) {
            throw invalid("HTTP parameter values must be scalar");
        }
        String value = String.valueOf(rawValue);
        if (template != null) {
            value = template.replace("{value}", value); // One pass; braces in a user's value are literal.
        }
        if (binding.header()) {
            if (value.chars().anyMatch(c -> c < 32 || c == 127)) {
                throw invalid("Invalid HTTP header value");
            }
            if (!headerNames.add(binding.name().toLowerCase(Locale.ROOT))) {
                throw invalid("Duplicate HTTP header binding");
            }
            headers.put(binding.name(), value);
        } else {
            if (query.containsKey(binding.name()) && (explicit || explicitQueryNames.contains(binding.name()))) {
                throw invalid("Duplicate HTTP query binding");
            }
            query.put(binding.name(), value); // Preserve the legacy user's override of a preset query value.
            if (explicit) {
                explicitQueryNames.add(binding.name());
            }
        }
    }

    private static Binding binding(String stableName, String type, String name) {
        if (stableName == null || stableName.isBlank()) {
            throw invalid("Missing parameter name");
        }
        String resolvedType = type == null ? "query" : type;
        if (!"query".equals(resolvedType) && !"header".equals(resolvedType)) {
            throw invalid("Invalid HTTP binding type");
        }
        String resolvedName = name == null ? stableName : name;
        if (resolvedName.isBlank()) {
            throw invalid("Missing HTTP binding name");
        }
        boolean header = "header".equals(resolvedType);
        if (header && !resolvedName.matches("[!#$%&'*+.^_`|~0-9A-Za-z-]+")) {
            throw invalid("Invalid HTTP header name");
        }
        return new Binding(header, resolvedName);
    }

    private static void validateTemplate(String template) {
        if (template != null && (template.isBlank() || !template.contains("{value}")
                || template.contains("${") || template.chars().anyMatch(c -> c < 32 || c == 127)
                || template.replace("{value}", "").matches("(?s).*[{}].*"))) {
            throw invalid("Invalid HTTP value template");
        }
    }

    private static String appendQuery(String baseUrl, Map<String, String> query) {
        URI uri;
        try {
            uri = URI.create(baseUrl);
        } catch (RuntimeException ex) {
            throw invalid("Invalid MCP HTTP URL");
        }
        if (uri.getHost() == null || !("http".equals(uri.getScheme()) || "https".equals(uri.getScheme()))
                || uri.getRawUserInfo() != null) {
            throw invalid("Invalid MCP HTTP URL");
        }
        if (query.isEmpty()) {
            return baseUrl;
        }
        int fragmentIndex = baseUrl.indexOf('#');
        String prefix = fragmentIndex < 0 ? baseUrl : baseUrl.substring(0, fragmentIndex);
        String fragment = fragmentIndex < 0 ? "" : baseUrl.substring(fragmentIndex);
        StringBuilder result = new StringBuilder(prefix);
        result.append(prefix.endsWith("?") || prefix.endsWith("&") ? "" : prefix.contains("?") ? "&" : "?");
        boolean first = true;
        for (Map.Entry<String, String> param : query.entrySet()) {
            if (!first) {
                result.append('&');
            }
            first = false;
            result.append(encode(param.getKey())).append('=').append(encode(param.getValue()));
        }
        return result.append(fragment).toString();
    }

    private static String encode(String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8).replace("+", "%20");
    }

    private static IllegalArgumentException invalid(String reason) {
        return new IllegalArgumentException(reason); // Never include configuration or credential values.
    }

    private static <T> List<T> orEmpty(List<T> values) {
        return values == null ? List.of() : values;
    }

    private record Binding(boolean header, String name) {
    }
}
