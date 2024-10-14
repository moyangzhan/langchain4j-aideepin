package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseEmbeddingMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Slf4j
@Service
public class KnowledgeBaseEmbeddingService extends ServiceImpl<KnowledgeBaseEmbeddingMapper, KnowledgeBaseEmbedding> {

    public List<KnowledgeBaseEmbedding> listByEmbeddingIds(List<String> embeddingIds) {
        LambdaQueryWrapper<KnowledgeBaseEmbedding> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.in(KnowledgeBaseEmbedding::getEmbeddingId, embeddingIds.stream().map(UUID::fromString).toList());
        return baseMapper.selectList(lambdaQueryWrapper);
    }

    public Page<KbItemEmbeddingDto> listByItemUuid(String kbItemUuid, int currentPage, int pageSize) {
        Page<KnowledgeBaseEmbedding> sourcePage = baseMapper.selectByItemUuid(new Page<>(currentPage, pageSize), kbItemUuid);
        Page<KbItemEmbeddingDto> result = new Page<>();
        MPPageUtil.convertToPage(sourcePage, result, KbItemEmbeddingDto.class, (source, target) -> {
            target.setEmbedding(source.getEmbedding().toArray());
            return target;
        });
        return result;
    }

    /**
     * 删除{kbItemUuid}这个知识库条目的向量
     *
     * @param kbItemUuid 知识库条目uuid
     * @return
     */
    public boolean deleteByItemUuid(String kbItemUuid) {
        return baseMapper.deleteByItemUuid(kbItemUuid);
    }

    public Integer countByKbUuid(String kbUuid) {
        return baseMapper.countByKbUuid(kbUuid);
    }
}
