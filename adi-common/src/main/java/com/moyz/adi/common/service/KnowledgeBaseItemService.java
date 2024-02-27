package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
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
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class KnowledgeBaseItemService extends ServiceImpl<KnowledgeBaseItemMapper, KnowledgeBaseItem> {

    @Resource
    private RAGService ragService;

    @Resource
    private KnowledgeBaseEmbeddingService knowledgeBaseEmbeddingService;

    @Lazy
    @Resource
    private KnowledgeBaseService knowledgeBaseService;

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

    public Page<KnowledgeBaseItem> search(String kbUuid, String keyword, Integer currentPage, Integer pageSize) {
        LambdaQueryChainWrapper<KnowledgeBaseItem> wrapper = ChainWrappers.lambdaQueryChain(baseMapper);
        wrapper.select(KnowledgeBaseItem::getId, KnowledgeBaseItem::getUuid, KnowledgeBaseItem::getTitle, KnowledgeBaseItem::getBrief, KnowledgeBaseItem::getKbUuid, KnowledgeBaseItem::getIsEmbedded, KnowledgeBaseItem::getCreateTime, KnowledgeBaseItem::getUpdateTime);
        wrapper.eq(KnowledgeBaseItem::getIsDeleted, false);
        wrapper.eq(KnowledgeBaseItem::getKbUuid, kbUuid);
        if (StringUtils.isNotBlank(keyword)) {
            wrapper.eq(KnowledgeBaseItem::getTitle, keyword);
        }
        return wrapper.page(new Page<>(currentPage, pageSize));
    }

    public boolean checkAndEmbedding(String[] uuids) {
        if (ArrayUtils.isEmpty(uuids)) {
            return false;
        }
        for (String uuid : uuids) {
            checkAndEmbedding(uuid);
        }
        return true;
    }

    public boolean checkAndEmbedding(String uuid) {
        if (checkPrivilege(uuid)) {
            KnowledgeBaseItem one = getEnable(uuid);
            return embedding(one);
        }
        return false;
    }


    /**
     * 知识点向量化，如向量已存在，则先删除
     *
     * @param kbItem
     * @return
     */
    public boolean embedding(KnowledgeBaseItem kbItem) {
        knowledgeBaseEmbeddingService.deleteByItemUuid(kbItem.getUuid());

        Metadata metadata = new Metadata();
        metadata.add("kb_uuid", kbItem.getKbUuid());
        metadata.add("kb_item_uuid", kbItem.getUuid());
        Document document = new Document(kbItem.getRemark(), metadata);
        ragService.ingest(document);
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
        return true;
    }

    private boolean checkPrivilege(String uuid) {
        if (StringUtils.isBlank(uuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        if (user.getIsAdmin()) {
            return true;
        }
        Optional<KnowledgeBaseItem> kbItem = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBaseItem::getUuid, uuid)
                .oneOpt();
        if (kbItem.isPresent()) {
            KnowledgeBase kb = knowledgeBaseService.getById(kbItem.get().getKbId());
            if (null != kb) {
                return kb.getOwnerId().equals(user.getId());
            }
        }
        return false;
    }
}
