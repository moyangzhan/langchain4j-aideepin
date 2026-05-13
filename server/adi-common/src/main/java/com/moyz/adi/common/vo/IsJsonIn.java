package com.moyz.adi.common.vo;

import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.store.embedding.filter.Filter;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import static dev.langchain4j.internal.ValidationUtils.*;
import static java.util.Collections.unmodifiableSet;

@ToString
@EqualsAndHashCode
public class IsJsonIn implements Filter {

    private final String key;
    private final Collection<?> comparisonValues;

    public IsJsonIn(String key, Collection<?> comparisonValues) {
        this.key = ensureNotBlank(key, "key");
        Set<?> copy = new HashSet<>(ensureNotEmpty(comparisonValues, "comparisonValues with key '" + key + "'"));
        comparisonValues.forEach(value -> ensureNotNull(value, "comparisonValue with key '" + key + "'"));
        this.comparisonValues = unmodifiableSet(copy);
    }

    public String key() {
        return key;
    }

    public Collection<?> comparisonValues() {
        return comparisonValues;
    }

    @Override
    public boolean test(Object object) {
        if (!(object instanceof Metadata)) {
            return false;
        }
        Metadata metadata = (Metadata) object;
        if (!metadata.containsKey(key)) {
            return false;
        }
        return comparisonValues.contains(metadata.toMap().get(key));
    }
}
