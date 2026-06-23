package com.moyz.adi.common.service.embedding;

import com.moyz.adi.common.dto.KbItemEmbeddingDto;

import java.util.List;

/**
 * Reverse-lookup access to the dedicated episodic memory vector store. Parallel to
 * {@link ICharacterMemoryEmbeddingService} which serves the semantic store.
 * <p>
 * 情景记忆向量库的反查接口，与 semantic 反查接口 {@link ICharacterMemoryEmbeddingService} 平行。
 */
public interface IEpisodicMemoryEmbeddingService {

    List<KbItemEmbeddingDto> listByEmbeddingIds(List<String> embeddingIds);

    /**
     * Timeline retrieval: list a character's most recent episodic events, sorted by
     * the {@code created_at} field carried in vector store metadata. Vector
     * similarity is not used — this is a metadata-only query against the
     * physically isolated episodic store.
     * <p>
     * 时间轴检索：按 metadata 中的 {@code created_at} 倒序，返回某角色最近的 N 条情景事件。
     * 不走相似度检索，仅基于 metadata 在物理隔离的情景表上查询。
     *
     * @param characterId 角色 ID
     * @param limit       返回的最大条数
     * @return 按时间倒序的 episodic 行
     */
    List<KbItemEmbeddingDto> listRecentByCharacter(Long characterId, int limit);
}

