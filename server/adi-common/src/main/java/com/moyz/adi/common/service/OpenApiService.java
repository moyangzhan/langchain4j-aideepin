package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ApiKeyResp;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.AesUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.Set;

import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * OpenAPI Key management service.
 * Supports generating, retrieving (masked), revealing, and validating API keys
 * for conversations, knowledge bases, and workflows.
 */
@Slf4j
@Service
public class OpenApiService {

    private static final String KEY_PREFIX = "adi-";
    private static final Set<String> VALID_TYPES = Set.of("conv", "kb", "wf");

    @Resource
    private ConversationService conversationService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private WorkflowService workflowService;

    @Resource
    private UserService userService;

    /**
     * Generate an API key for the specified resource.
     * The caller must be the resource owner or an admin.
     *
     * @param type resource type: conv / kb / wf
     * @param uuid resource uuid
     * @return ApiKeyResp containing the raw key and masked key
     */
    public ApiKeyResp generateApiKey(String type, String uuid) {
        validateType(type);
        User currentUser = ThreadContext.getCurrentUser();
        String rawKey = KEY_PREFIX + AesUtil.generateRandomKey();
        String encryptedValue = AesUtil.encrypt(rawKey);

        switch (type) {
            case "conv" -> {
                Conversation conv = getConvOrThrow(uuid);
                checkOwnership(conv.getUserId(), currentUser);
                conversationService.lambdaUpdate()
                        .eq(Conversation::getId, conv.getId())
                        .set(Conversation::getApiKey, encryptedValue)
                        .update();
            }
            case "kb" -> {
                KnowledgeBase kb = getKbOrThrow(uuid);
                checkOwnership(kb.getOwnerId(), currentUser);
                knowledgeBaseService.lambdaUpdate()
                        .eq(KnowledgeBase::getId, kb.getId())
                        .set(KnowledgeBase::getApiKey, encryptedValue)
                        .update();
            }
            case "wf" -> {
                Workflow wf = getWfOrThrow(uuid);
                checkOwnership(wf.getUserId(), currentUser);
                ChainWrappers.lambdaUpdateChain(workflowService.getBaseMapper())
                        .eq(Workflow::getId, wf.getId())
                        .set(Workflow::getApiKey, encryptedValue)
                        .update();
            }
            default -> throw new BaseException(A_PARAMS_ERROR);
        }

        log.info("API key generated for {} uuid:{}, userId:{}", type, uuid, currentUser.getId());
        return ApiKeyResp.builder()
                .rawKey(rawKey)
                .maskedKey(maskKey(rawKey))
                .build();
    }

    /**
     * Get API key info in masked form.
     *
     * @param type resource type: conv / kb / wf
     * @param uuid resource uuid
     * @return ApiKeyResp with masked key (rawKey is null), or null if no key set
     */
    public ApiKeyResp getApiKeyInfo(String type, String uuid) {
        validateType(type);
        User currentUser = ThreadContext.getCurrentUser();

        String encryptedApiKey;
        switch (type) {
            case "conv" -> {
                Conversation conv = getConvOrThrow(uuid);
                checkOwnership(conv.getUserId(), currentUser);
                encryptedApiKey = conv.getApiKey();
            }
            case "kb" -> {
                KnowledgeBase kb = getKbOrThrow(uuid);
                checkOwnership(kb.getOwnerId(), currentUser);
                encryptedApiKey = kb.getApiKey();
            }
            case "wf" -> {
                Workflow wf = getWfOrThrow(uuid);
                checkOwnership(wf.getUserId(), currentUser);
                encryptedApiKey = wf.getApiKey();
            }
            default -> throw new BaseException(A_PARAMS_ERROR);
        }

        if (StringUtils.isBlank(encryptedApiKey)) {
            return null;
        }
        String rawKey = AesUtil.decrypt(encryptedApiKey);
        return ApiKeyResp.builder()
                .rawKey(null)
                .maskedKey(maskKey(rawKey))
                .build();
    }

    /**
     * Reveal the full API key.
     * The caller must be the resource owner or an admin.
     *
     * @param type resource type: conv / kb / wf
     * @param uuid resource uuid
     * @return ApiKeyResp containing both raw and masked key
     */
    public ApiKeyResp revealApiKey(String type, String uuid) {
        validateType(type);
        User currentUser = ThreadContext.getCurrentUser();

        String encryptedApiKey;
        switch (type) {
            case "conv" -> {
                Conversation conv = getConvOrThrow(uuid);
                checkOwnership(conv.getUserId(), currentUser);
                encryptedApiKey = conv.getApiKey();
            }
            case "kb" -> {
                KnowledgeBase kb = getKbOrThrow(uuid);
                checkOwnership(kb.getOwnerId(), currentUser);
                encryptedApiKey = kb.getApiKey();
            }
            case "wf" -> {
                Workflow wf = getWfOrThrow(uuid);
                checkOwnership(wf.getUserId(), currentUser);
                encryptedApiKey = wf.getApiKey();
            }
            default -> throw new BaseException(A_PARAMS_ERROR);
        }

        if (StringUtils.isBlank(encryptedApiKey)) {
            throw new BaseException(A_API_KEY_NOT_FOUND);
        }

        String rawKey = AesUtil.decrypt(encryptedApiKey);
        return ApiKeyResp.builder()
                .rawKey(rawKey)
                .maskedKey(maskKey(rawKey))
                .build();
    }

    /**
     * Validate an API key from an external request.
     * Encrypts the raw key and searches the corresponding table for a match.
     *
     * @param rawKey the API key from the request header
     * @param type   resource type: conv / kb / wf
     * @return ValidateResult containing the found entity uuid and the owner user
     */
    public ValidateResult validateApiKey(String rawKey, String type) {
        validateType(type);
        if (StringUtils.isBlank(rawKey) || !rawKey.startsWith(KEY_PREFIX)) {
            throw new BaseException(A_API_KEY_INVALID);
        }

        String encryptedValue = AesUtil.encrypt(rawKey);

        switch (type) {
            case "conv" -> {
                Conversation conv = conversationService.lambdaQuery()
                        .eq(Conversation::getApiKey, encryptedValue)
                        .eq(Conversation::getIsDeleted, false)
                        .one();
                if (null == conv) {
                    throw new BaseException(A_API_KEY_NOT_FOUND);
                }
                User owner = userService.getById(conv.getUserId());
                if (null == owner) {
                    throw new BaseException(A_API_KEY_INVALID);
                }
                return new ValidateResult(conv.getUuid(), owner);
            }
            case "kb" -> {
                KnowledgeBase kb = knowledgeBaseService.lambdaQuery()
                        .eq(KnowledgeBase::getApiKey, encryptedValue)
                        .eq(KnowledgeBase::getIsDeleted, false)
                        .one();
                if (null == kb) {
                    throw new BaseException(A_API_KEY_NOT_FOUND);
                }
                User owner = userService.getById(kb.getOwnerId());
                if (null == owner) {
                    throw new BaseException(A_API_KEY_INVALID);
                }
                return new ValidateResult(kb.getUuid(), owner);
            }
            case "wf" -> {
                Workflow wf = ChainWrappers.lambdaQueryChain(workflowService.getBaseMapper())
                        .eq(Workflow::getApiKey, encryptedValue)
                        .eq(Workflow::getIsDeleted, false)
                        .one();
                if (null == wf) {
                    throw new BaseException(A_API_KEY_NOT_FOUND);
                }
                User owner = userService.getById(wf.getUserId());
                if (null == owner) {
                    throw new BaseException(A_API_KEY_INVALID);
                }
                return new ValidateResult(wf.getUuid(), owner);
            }
            default -> throw new BaseException(A_PARAMS_ERROR);
        }
    }

    // ========== Private helpers ==========

    private void validateType(String type) {
        if (StringUtils.isBlank(type) || !VALID_TYPES.contains(type)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
    }

    private void checkOwnership(Long resourceUserId, User currentUser) {
        if (!currentUser.getIsAdmin() && !currentUser.getId().equals(resourceUserId)) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

    private Conversation getConvOrThrow(String uuid) {
        return conversationService.lambdaQuery()
                .eq(Conversation::getUuid, uuid)
                .eq(Conversation::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_CONVERSATION_NOT_EXIST));
    }

    private KnowledgeBase getKbOrThrow(String uuid) {
        return knowledgeBaseService.lambdaQuery()
                .eq(KnowledgeBase::getUuid, uuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_DATA_NOT_FOUND));
    }

    private Workflow getWfOrThrow(String uuid) {
        return workflowService.getByUuid(uuid);
        // WorkflowService.getByUuid already handles not-found via getOrThrow
    }

    /**
     * Generate masked version of the key: "adi-a3f8****f6g7"
     * Keeps the first 8 characters (including prefix) and last 4 characters.
     *
     * @param rawKey the plain API key
     * @return masked key string
     */
    private String maskKey(String rawKey) {
        if (StringUtils.isBlank(rawKey) || rawKey.length() <= 12) {
            return "****";
        }
        return rawKey.substring(0, 8) + "****" + rawKey.substring(rawKey.length() - 4);
    }

    /**
     * Result of API key validation, carrying the entity uuid and the owner user.
     */
    public record ValidateResult(String entityUuid, User ownerUser) {
    }
}
