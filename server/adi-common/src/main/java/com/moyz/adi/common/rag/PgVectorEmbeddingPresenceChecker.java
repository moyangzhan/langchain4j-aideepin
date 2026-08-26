package com.moyz.adi.common.rag;

import com.moyz.adi.common.config.AdiProperties;
import com.moyz.adi.common.util.AdiPropertiesUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import java.sql.PreparedStatement;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * pgvector presence check: queries the active embedding table directly.
 * The table name must match {@code PgVectorEmbeddingStoreConfig}
 * (adi_knowledge_base_embedding + model suffix).
 */
@Slf4j
@Component
@ConditionalOnProperty(value = "adi.vector-database", havingValue = "pgvector")
public class PgVectorEmbeddingPresenceChecker implements EmbeddingPresenceChecker {

    private static final int BATCH_SIZE = 500;

    private final JdbcTemplate jdbcTemplate;
    private final String tableName;

    public PgVectorEmbeddingPresenceChecker(JdbcTemplate jdbcTemplate, AdiProperties adiProperties) {
        this.jdbcTemplate = jdbcTemplate;
        Pair<String, Integer> pair = AdiPropertiesUtil.getSuffixAndDimension(adiProperties);
        this.tableName = "adi_knowledge_base_embedding" +
                (StringUtils.isNotBlank(pair.getLeft()) ? "_" + pair.getLeft() : "");
    }

    @Override
    public Set<String> findExisting(Collection<String> embeddingIds) {
        Set<String> existing = new HashSet<>();
        List<String> ids = embeddingIds.stream().filter(StringUtils::isNotBlank).toList();
        for (int from = 0; from < ids.size(); from += BATCH_SIZE) {
            List<String> batch = ids.subList(from, Math.min(ids.size(), from + BATCH_SIZE));
            String placeholders = String.join(",", java.util.Collections.nCopies(batch.size(), "?"));
            String sql = "SELECT CAST(embedding_id AS VARCHAR) FROM " + tableName +
                    " WHERE CAST(embedding_id AS VARCHAR) IN (" + placeholders + ")";
            existing.addAll(jdbcTemplate.query(con -> {
                PreparedStatement ps = con.prepareStatement(sql);
                for (int i = 0; i < batch.size(); i++) {
                    ps.setString(i + 1, batch.get(i));
                }
                return ps;
            }, (rs, rowNum) -> rs.getString(1)));
        }
        return existing;
    }
}
