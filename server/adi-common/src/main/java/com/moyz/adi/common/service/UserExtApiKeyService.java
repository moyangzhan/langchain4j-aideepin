package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.UserExtApiKey;
import com.moyz.adi.common.mapper.UserExtApiKeyMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class UserExtApiKeyService extends ServiceImpl<UserExtApiKeyMapper, UserExtApiKey> {

    /**
     * 根据用户ID和资源类型获取API Key记录
     */
    public UserExtApiKey getByUserAndType(Long userId, String resourceType) {
        return this.lambdaQuery()
                .eq(UserExtApiKey::getUserId, userId)
                .eq(UserExtApiKey::getResourceType, resourceType)
                .eq(UserExtApiKey::getIsDeleted, false)
                .one();
    }

    /**
     * 根据加密后的Key和资源类型获取API Key记录
     */
    public UserExtApiKey getByEncryptedKey(String resourceType, String encryptedKey) {
        return this.lambdaQuery()
                .eq(UserExtApiKey::getResourceType, resourceType)
                .eq(UserExtApiKey::getApiKey, encryptedKey)
                .eq(UserExtApiKey::getIsDeleted, false)
                .one();
    }

    /**
     * 新增或更新用户的API Key
     */
    public void saveOrUpdate(Long userId, String resourceType, String encryptedKey) {
        UserExtApiKey existing = getByUserAndType(userId, resourceType);

        if (existing != null) {
            this.lambdaUpdate()
                    .eq(UserExtApiKey::getId, existing.getId())
                    .set(UserExtApiKey::getApiKey, encryptedKey)
                    .update();
        } else {
            UserExtApiKey newRecord = new UserExtApiKey();
            newRecord.setUserId(userId);
            newRecord.setResourceType(resourceType);
            newRecord.setApiKey(encryptedKey);
            this.save(newRecord);
        }
    }
}
