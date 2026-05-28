package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.ApiKeyResp;
import com.moyz.adi.common.service.ExtApiService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.web.bind.annotation.*;

@Tag(name = "External API Key controller", description = "API Key management for External API access")
@RestController
@RequestMapping("/external-api-key")
public class ExtApiKeyController {

    @Resource
    private ExtApiService extApiService;

    // ========== 资源级 API Key ==========

    @Operation(summary = "Generate API Key for a resource | 为资源生成 API Key")
    @PostMapping("/{type}/{uuid}")
    public ApiKeyResp generate(@PathVariable String type, @PathVariable String uuid) {
        return extApiService.generateApiKey(type, uuid);
    }

    @Operation(summary = "Get API Key info (masked) | 获取 API Key 信息（掩码）")
    @GetMapping("/{type}/{uuid}")
    public ApiKeyResp info(@PathVariable String type, @PathVariable String uuid) {
        return extApiService.getApiKeyInfo(type, uuid);
    }

    @Operation(summary = "Reveal API Key (plaintext) | 查看 API Key 明文")
    @PostMapping("/{type}/{uuid}/reveal")
    public ApiKeyResp reveal(@PathVariable String type, @PathVariable String uuid) {
        return extApiService.revealApiKey(type, uuid);
    }

    // ========== 用户级 API Key ==========

    @Operation(summary = "Generate user-level API Key | 为用户生成 API Key")
    @PostMapping("/user/{type}")
    public ApiKeyResp generateUserKey(@PathVariable String type) {
        return extApiService.generateUserApiKey(type);
    }

    @Operation(summary = "Get user-level API Key info (masked) | 获取用户 API Key 信息（掩码）")
    @GetMapping("/user/{type}")
    public ApiKeyResp userInfo(@PathVariable String type) {
        return extApiService.getUserApiKeyInfo(type);
    }

    @Operation(summary = "Reveal user-level API Key (plaintext) | 查看用户 API Key 明文")
    @PostMapping("/user/{type}/reveal")
    public ApiKeyResp revealUserKey(@PathVariable String type) {
        return extApiService.revealUserApiKey(type);
    }
}
