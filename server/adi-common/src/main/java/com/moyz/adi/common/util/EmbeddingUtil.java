package com.moyz.adi.common.util;

import com.moyz.adi.common.dto.KbDocumentEmbeddingDto;
import com.moyz.adi.common.dto.RefEmbeddingDto;

import java.util.ArrayList;
import java.util.List;

public class EmbeddingUtil {

    public static List<RefEmbeddingDto> itemToRefEmbeddingDto(List<KbDocumentEmbeddingDto> embeddings) {
        List<RefEmbeddingDto> result = new ArrayList<>();
        for (KbDocumentEmbeddingDto embedding : embeddings) {
            RefEmbeddingDto newOne = RefEmbeddingDto.builder()
                    .embeddingId(embedding.getEmbeddingId())
                    .text(embedding.getText())
                    .build();
            result.add(newOne);
        }
        return result;
    }

}
