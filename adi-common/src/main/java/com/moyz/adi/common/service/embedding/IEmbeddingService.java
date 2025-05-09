package com.moyz.adi.common.service.embedding;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbItemEmbeddingDto;

import java.util.List;

public interface IEmbeddingService {
    List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds);

    Page<KbItemEmbeddingDto> listByItemUuid(String kbItemUuid, int currentPage, int pageSize);

    boolean deleteByItemUuid(String kbItemUuid);

    Integer countByKbUuid(String kbUuid);
}
