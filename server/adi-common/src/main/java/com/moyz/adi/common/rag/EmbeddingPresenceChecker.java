package com.moyz.adi.common.rag;

import java.util.Collection;
import java.util.Set;

/**
 * Checks which embedding ids actually exist in the vector store.
 * Used to detect drift between status columns and the store
 * (e.g. after a database restore or external deletion).
 */
public interface EmbeddingPresenceChecker {

    /**
     * @param embeddingIds ids to verify
     * @return the subset present in the store
     */
    Set<String> findExisting(Collection<String> embeddingIds);
}
