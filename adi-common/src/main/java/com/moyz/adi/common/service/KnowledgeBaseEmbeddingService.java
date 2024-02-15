package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;
import com.moyz.adi.common.entity.KnowledgeBaseEmbedding;
import com.moyz.adi.common.mapper.KnowledgeBaseEmbeddingMapper;
import com.moyz.adi.common.util.MPPageUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class KnowledgeBaseEmbeddingService extends ServiceImpl<KnowledgeBaseEmbeddingMapper, KnowledgeBaseEmbedding> {

    public Page<KbItemEmbeddingDto> listByItemUuid(String kbItemUuid, int currentPage, int pageSize) {
        Page<KnowledgeBaseEmbedding> sourcePage = baseMapper.selectByItemUuid(new Page<>(currentPage, pageSize), kbItemUuid);
        Page<KbItemEmbeddingDto> result = new Page<>();
        MPPageUtil.convertTo(sourcePage, result, KbItemEmbeddingDto.class, (source, target) -> {
            target.setEmbedding(source.getEmbedding().toArray());
            return target;
        });
        return result;
    }

    public boolean deleteByItemUuid(String kbItemUuid){
        return baseMapper.deleteByItemUuid(kbItemUuid);
    }
}
