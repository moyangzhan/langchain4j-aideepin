package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ApiKeyResp;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.enums.ExtApiResourceType;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.AesUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * External API Key management service.
 * Supports generating, retrieving (masked), revealing, and validating API keys
 * for characters, knowledge bases, and workflows.
 */
@Slf4j
@Service
public class ExtApiService {

    private static final String KEY_PREFIX = "ext-";

    @Resource
    private CharacterService characterService;

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
     * @param type resource type
     * @param uuid resource uuid
     * @return ApiKeyResp containing the raw key and masked key
     */
    public ApiKeyResp generateApiKey(String type, String uuid) {
        ExtApiResourceType resourceType = parseTypeOrThrow(type);
        User currentUser = ThreadContext.getCurrentUser();
        String rawKey = KEY_PREFIX + AesUtil.generateRandomKey();
        String encryptedValue = AesUtil.encrypt(rawKey);

        switch (resourceType) {
            case CHARACTER -> {
                Character character = getCharacterOrThrow(uuid);
                checkOwnership(character.getUserId(), currentUser);
                characterService.lambdaUpdate()
                        .eq(Character::getId, character.getId())
                        .set(Character::getApiKey, encryptedValue)
                        .update();
            }
            case KNOWLEDGE -> {
                KnowledgeBase kb = getKbOrThrow(uuid);
                checkOwnership(kb.getOwnerId(), currentUser);
                knowledgeBaseService.lambdaUpdate()
                        .eq(KnowledgeBase::getId, kb.getId())
                        .set(KnowledgeBase::getApiKey, encryptedValue)
                        .update();
            }
            case WORKFLOW -> {
                Workflow wf = getWfOrThrow(uuid);
                checkOwnership(wf.getUserId(), currentUser);
                ChainWrappers.lambdaUpdateChain(workflowService.getBaseMapper())
                        .eq(Workflow::getId, wf.getId())
                        .set(Workflow::getApiKey, encryptedValue)
                        .update();
            }
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
     * @param type resource type
     * @param uuid resource uuid
     * @return ApiKeyResp with masked key (rawKey is null), or null if no key set
     */
    public ApiKeyResp getApiKeyInfo(String type, String uuid) {
        ExtApiResourceType resourceType = parseTypeOrThrow(type);
        User currentUser = ThreadContext.getCurrentUser();

        String encryptedApiKey = resolveApiKeyNoAuth(resourceType, uuid);
        boolean canManage = checkOwnershipSilent(resourceType, uuid, currentUser);

        if (StringUtils.isBlank(encryptedApiKey)) {
            return ApiKeyResp.builder().canManage(canManage).build();
        }
        String rawKey = AesUtil.decrypt(encryptedApiKey);
        return ApiKeyResp.builder()
                .rawKey(null)
                .maskedKey(maskKey(rawKey))
                .canManage(canManage)
                .build();
    }

    /**
     * Reveal the full API key.
     * The caller must be the resource owner or an admin.
     *
     * @param type resource type
     * @param uuid resource uuid
     * @return ApiKeyResp containing both raw and masked key
     */
    public ApiKeyResp revealApiKey(String type, String uuid) {
        ExtApiResourceType resourceType = parseTypeOrThrow(type);
        User currentUser = ThreadContext.getCurrentUser();

        String encryptedApiKey = resolveApiKey(resourceType, uuid, currentUser);

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
     * @param type   resource type
     * @return ValidateResult containing the found entity uuid and the owner user
     */
    public ValidateResult validateApiKey(String rawKey, String type) {
        ExtApiResourceType resourceType = parseTypeOrThrow(type);
        if (StringUtils.isBlank(rawKey) || !rawKey.startsWith(KEY_PREFIX)) {
            throw new BaseException(A_API_KEY_INVALID);
        }

        String encryptedValue = AesUtil.encrypt(rawKey);

        return switch (resourceType) {
            case CHARACTER -> {
                Character character = characterService.lambdaQuery()
                        .eq(Character::getApiKey, encryptedValue)
                        .eq(Character::getIsDeleted, false)
                        .one();
                if (null == character) {
                    throw new BaseException(A_API_KEY_NOT_FOUND);
                }
                User owner = userService.getById(character.getUserId());
                if (null == owner) {
                    throw new BaseException(A_API_KEY_INVALID);
                }
                yield new ValidateResult(character.getUuid(), owner);
            }
            case KNOWLEDGE -> {
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
                yield new ValidateResult(kb.getUuid(), owner);
            }
            case WORKFLOW -> {
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
                yield new ValidateResult(wf.getUuid(), owner);
            }
        };
    }

    // ========== Private helpers ==========

    private ExtApiResourceType parseTypeOrThrow(String type) {
        ExtApiResourceType resourceType = ExtApiResourceType.fromValue(type);
        if (null == resourceType) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        return resourceType;
    }

    /**
     * Resolve the encrypted API key for the given resource, checking ownership.
     */
    private String resolveApiKey(ExtApiResourceType resourceType, String uuid, User currentUser) {
        return switch (resourceType) {
            case CHARACTER -> {
                Character character = getCharacterOrThrow(uuid);
                checkOwnership(character.getUserId(), currentUser);
                yield character.getApiKey();
            }
            case KNOWLEDGE -> {
                KnowledgeBase kb = getKbOrThrow(uuid);
                checkOwnership(kb.getOwnerId(), currentUser);
                yield kb.getApiKey();
            }
            case WORKFLOW -> {
                Workflow wf = getWfOrThrow(uuid);
                checkOwnership(wf.getUserId(), currentUser);
                yield wf.getApiKey();
            }
        };
    }

    private void checkOwnership(Long resourceUserId, User currentUser) {
        if (!currentUser.getIsAdmin() && !currentUser.getId().equals(resourceUserId)) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

    private boolean checkOwnershipSilent(ExtApiResourceType resourceType, String uuid, User currentUser) {
        if (currentUser.getIsAdmin()) {
            return true;
        }
        Long resourceUserId = switch (resourceType) {
            case CHARACTER -> getCharacterOrThrow(uuid).getUserId();
            case KNOWLEDGE -> getKbOrThrow(uuid).getOwnerId();
            case WORKFLOW -> getWfOrThrow(uuid).getUserId();
        };
        return currentUser.getId().equals(resourceUserId);
    }

    private String resolveApiKeyNoAuth(ExtApiResourceType resourceType, String uuid) {
        return switch (resourceType) {
            case CHARACTER -> getCharacterOrThrow(uuid).getApiKey();
            case KNOWLEDGE -> getKbOrThrow(uuid).getApiKey();
            case WORKFLOW -> getWfOrThrow(uuid).getApiKey();
        };
    }

    private Character getCharacterOrThrow(String uuid) {
        return characterService.lambdaQuery()
                .eq(Character::getUuid, uuid)
                .eq(Character::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_CHARACTER_NOT_EXIST));
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
    }

    /**
     * Generate masked version of the key: "ext-a3f8****f6g7"
     * Keeps the first 8 characters (including prefix) and last 4 characters.
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
