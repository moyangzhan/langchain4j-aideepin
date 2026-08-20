package com.moyz.adi.common.service.embedding;

import com.moyz.adi.common.dto.KbDocumentEmbeddingDto;

import java.util.List;

public interface ICharacterMemoryEmbeddingService {
    List<KbDocumentEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds);

    void incrementHitCount(List<String> embeddingIds);
}
