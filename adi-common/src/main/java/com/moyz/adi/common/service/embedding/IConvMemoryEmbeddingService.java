package com.moyz.adi.common.service.embedding;

import com.moyz.adi.common.dto.KbItemEmbeddingDto;

import java.util.List;

public interface IConvMemoryEmbeddingService {
    List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds);
}
