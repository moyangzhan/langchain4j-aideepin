package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.ApiKeyResp;
import com.moyz.adi.common.service.OpenApiService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.web.bind.annotation.*;

@Tag(name = "External API Key controller", description = "API Key management for External API access")
@RestController
@RequestMapping("/external-api-key")
public class OpenApiKeyController {

    @Resource
    private OpenApiService openApiService;

    @Operation(summary = "Generate API Key for a resource | 为资源生成 API Key")
    @PostMapping("/{type}/{uuid}")
    public ApiKeyResp generate(@PathVariable String type, @PathVariable String uuid) {
        return openApiService.generateApiKey(type, uuid);
    }

    @Operation(summary = "Get API Key info (masked) | 获取 API Key 信息（掩码）")
    @GetMapping("/{type}/{uuid}")
    public ApiKeyResp info(@PathVariable String type, @PathVariable String uuid) {
        return openApiService.getApiKeyInfo(type, uuid);
    }

    @Operation(summary = "Reveal API Key (plaintext) | 查看 API Key 明文")
    @PostMapping("/{type}/{uuid}/reveal")
    public ApiKeyResp reveal(@PathVariable String type, @PathVariable String uuid) {
        return openApiService.revealApiKey(type, uuid);
    }
}
