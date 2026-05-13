package com.moyz.adi.common.vo;

import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.store.embedding.filter.Filter;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import static dev.langchain4j.internal.ValidationUtils.ensureNotBlank;

@ToString
@EqualsAndHashCode
public class GraphContains implements Filter {

    private final String key;
    private final String value;

    public GraphContains(String key, String value) {
        this.key = ensureNotBlank(key, "key");
        this.value = ensureNotBlank(value, "value");
    }

    public String key() {
        return key;
    }

    public String value() {
        return value;
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

        String actualValue = (String) metadata.toMap().get(key);
        return value.contains(actualValue);
    }
}
