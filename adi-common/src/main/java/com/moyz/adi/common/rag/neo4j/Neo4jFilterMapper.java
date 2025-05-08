package com.moyz.adi.common.rag.neo4j;

import com.aliyun.core.utils.StringUtils;
import com.moyz.adi.common.vo.GraphContains;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.EmbeddingStore;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.*;
import dev.langchain4j.store.embedding.filter.logical.And;
import dev.langchain4j.store.embedding.filter.logical.Not;
import dev.langchain4j.store.embedding.filter.logical.Or;

import java.util.AbstractMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.neo4j.cypherdsl.support.schema_name.SchemaNames.sanitize;

public class Neo4jFilterMapper {

    private String alias;

    private AdiNeo4jEmbeddingStore embeddingStore;

    public static final String UNSUPPORTED_FILTER_TYPE_ERROR = "Unsupported filter type: ";

    public static class IncrementalKeyMap {
        private final Map<String, Object> map = new ConcurrentHashMap<>();

        private int counter = 1;

        public String put(Object value) {
            String key = "param_" + counter++;
            map.put(key, value);
            return key;
        }

        public String put(String key, Object value) {
            map.put(key, value);
            return key;
        }

        public Map<String, Object> getMap() {
            return map;
        }
    }

    /**
     * For embedding store
     */
    public Neo4jFilterMapper(AdiNeo4jEmbeddingStore embeddingStore) {
        this.alias = "n";
        this.embeddingStore = embeddingStore;
    }

    /**
     * For graph store
     *
     * @param alias alias for the node
     */
    public Neo4jFilterMapper(String alias) {
        this.alias = StringUtils.isBlank(alias) ? "n" : alias;
    }

    final IncrementalKeyMap map = new IncrementalKeyMap();

    public AbstractMap.SimpleEntry<String, Map<?, ?>> map(Filter filter) {
        final String stringMapPair = getStringMapping(filter);
        return new AbstractMap.SimpleEntry<>(stringMapPair, map.getMap());
    }

    private String getStringMapping(Filter filter) {
        if (filter instanceof IsEqualTo item) {
            return getOperation(item.key(), "=", item.comparisonValue());
        } else if (filter instanceof IsNotEqualTo item) {
            return getOperation(item.key(), "<>", item.comparisonValue());
        } else if (filter instanceof IsGreaterThan item) {
            return getOperation(item.key(), ">", item.comparisonValue());
        } else if (filter instanceof IsGreaterThanOrEqualTo item) {
            return getOperation(item.key(), ">=", item.comparisonValue());
        } else if (filter instanceof IsLessThan item) {
            return getOperation(item.key(), "<", item.comparisonValue());
        } else if (filter instanceof IsLessThanOrEqualTo item) {
            return getOperation(item.key(), "<=", item.comparisonValue());
        } else if (filter instanceof IsIn item) {
            return mapIn(item);
        } else if (filter instanceof IsNotIn item) {
            return mapNotIn(item);
        } else if (filter instanceof And item) {
            return mapAnd(item);
        } else if (filter instanceof Not item) {
            return mapNot(item);
        } else if (filter instanceof Or item) {
            return mapOr(item);
        } else if (filter instanceof GraphContains filterInst) {
            return mapContains(filterInst);
        } else {
            throw new UnsupportedOperationException(
                    UNSUPPORTED_FILTER_TYPE_ERROR + filter.getClass().getName());
        }
    }

    private String getOperation(String key, String operator, Object value) {
        String completeKey = embeddingStore.getMetadataPrefix() + key;
        // put ($param_N, <value>) entry map
        final String param = map.put(alias + "_" + completeKey, value);

        String sanitizedKey = sanitize(completeKey).orElseThrow(() -> {
            String invalidSanitizeValue = String.format(
                    "The key %s, to assign to the operator %s and value %s, cannot be safely quoted",
                    completeKey, operator, value);
            return new RuntimeException(invalidSanitizeValue);
        });

        return alias + ".%s %s $%s".formatted(sanitizedKey, operator, param);
    }

    public IncrementalKeyMap getIncrementalKeyMap() {
        return map;
    }

    public String mapIn(IsIn filter) {
        return getOperation(filter.key(), "IN", filter.comparisonValues());
    }

    public String mapNotIn(IsNotIn filter) {
        final String inOperation = getOperation(filter.key(), "IN", filter.comparisonValues());
        return "NOT (%s)".formatted(inOperation);
    }

    private String mapAnd(And filter) {
        return "(%s) AND (%s)".formatted(getStringMapping(filter.left()), getStringMapping(filter.right()));
    }

    private String mapOr(Or filter) {
        return "(%s) OR (%s)".formatted(getStringMapping(filter.left()), getStringMapping(filter.right()));
    }

    private String mapNot(Not filter) {
        return "NOT (%s)".formatted(getStringMapping(filter.expression()));
    }

    private String mapContains(GraphContains filter) {
        return getOperation(filter.key(), "contains", filter.value());
    }
}