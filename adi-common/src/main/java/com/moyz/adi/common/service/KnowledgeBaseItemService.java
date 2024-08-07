package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.KbItemDto;
import com.moyz.adi.common.dto.KbItemEditReq;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseItemMapper;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.Metadata;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class KnowledgeBaseItemService extends ServiceImpl<KnowledgeBaseItemMapper, KnowledgeBaseItem> {

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private RAGService ragService;

    @Resource
    private KnowledgeBaseEmbeddingService knowledgeBaseEmbeddingService;

    public KnowledgeBaseItem saveOrUpdate(KbItemEditReq itemEditReq) {
        String uuid = itemEditReq.getUuid();
        KnowledgeBaseItem item = new KnowledgeBaseItem();
        item.setTitle(itemEditReq.getTitle());
        if (StringUtils.isNotBlank(itemEditReq.getBrief())) {
            item.setBrief(itemEditReq.getBrief());
        } else {
            item.setBrief(StringUtils.substring(itemEditReq.getRemark(), 0, 200));
        }
        item.setRemark(itemEditReq.getRemark());
        if (null == itemEditReq.getId() || itemEditReq.getId() < 1) {
            uuid = UUID.randomUUID().toString().replace("-", "");
            item.setUuid(uuid);
            item.setKbId(itemEditReq.getKbId());
            item.setKbUuid(itemEditReq.getKbUuid());
            baseMapper.insert(item);
        } else {
            item.setId(itemEditReq.getId());
            baseMapper.updateById(item);
        }

        stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, itemEditReq.getKbUuid());

        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .one();
    }

    public KnowledgeBaseItem getEnable(String uuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .one();
    }

    public Page<KbItemDto> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        return baseMapper.searchByKb(new Page<>(currentPage, pageSize), kbUuid, keyword);
    }

    public boolean checkAndEmbedding(KnowledgeBase knowledgeBase, String[] uuids) {
        if (ArrayUtils.isEmpty(uuids)) {
            return false;
        }
        for (String uuid : uuids) {
            checkAndEmbedding(knowledgeBase, uuid);
        }
        return true;
    }

    public boolean checkAndEmbedding(KnowledgeBase knowledgeBase, String uuid) {
        if (checkPrivilege(uuid)) {
            KnowledgeBaseItem item = getEnable(uuid);
            return embedding(knowledgeBase, item);
        }
        return false;
    }


    /**
     * 知识点向量化，如向量已存在，则先删除
     *
     * @param kbItem
     * @return
     */
    public boolean embedding(KnowledgeBase knowledgeBase, KnowledgeBaseItem kbItem) {
        knowledgeBaseEmbeddingService.deleteByItemUuid(kbItem.getUuid());

        Metadata metadata = new Metadata();
        metadata.put(AdiConstant.EmbeddingMetadataKey.KB_UUID, kbItem.getKbUuid());
        metadata.put(AdiConstant.EmbeddingMetadataKey.KB_ITEM_UUID, kbItem.getUuid());
        Document document = new Document(kbItem.getRemark(), metadata);
        ragService.ingest(document, knowledgeBase.getRagMaxOverlap());

        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseItem::getId, kbItem.getId())
                .set(KnowledgeBaseItem::getIsEmbedded, true)
                .update();

        stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, kbItem.getKbUuid());
        return true;
    }

    @Transactional
    public boolean softDelete(String uuid) {
        boolean privilege = checkPrivilege(uuid);
        if (!privilege) throw new BaseException(A_USER_NOT_AUTH);
        boolean success = ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .set(KnowledgeBaseItem::getIsDeleted, true)
                .update();
        if (!success) {
            return false;
        }
        knowledgeBaseEmbeddingService.deleteByItemUuid(uuid);

        KnowledgeBaseItem item = baseMapper.getByUuid(uuid);
        if (null != item) {
            stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, item.getKbUuid());
        }
        return true;
    }

    public int countByKbUuid(String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getKbUuid, kbUuid)
                .eq(KnowledgeBaseItem::getIsDeleted, false)
                .count()
                .intValue();
    }

    public int countTodayCreated() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime beginTime = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0, 0);
        LocalDateTime endTime = beginTime.plusDays(1);
        return baseMapper.countCreatedByTimePeriod(beginTime, endTime);
    }

    public int countAllCreated() {
        return baseMapper.countAllCreated();
    }

    private boolean checkPrivilege(String uuid) {
        if (StringUtils.isBlank(uuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            return true;
        }
        int belongToUser = baseMapper.belongToUser(uuid, user.getId());
        return belongToUser > 0;
    }
}
