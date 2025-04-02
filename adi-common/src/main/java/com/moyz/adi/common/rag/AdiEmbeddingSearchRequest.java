package com.moyz.adi.common.rag;

import dev.langchain4j.data.embedding.Embedding;
import dev.langchain4j.store.embedding.EmbeddingSearchRequest;
import dev.langchain4j.store.embedding.filter.Filter;

import java.util.List;

public class AdiEmbeddingSearchRequest extends EmbeddingSearchRequest {

    private List<String> ids;

    public AdiEmbeddingSearchRequest(List<String> ids, Embedding queryEmbedding, Integer maxResults, Double minScore, Filter filter) {
        super(queryEmbedding, maxResults, minScore, filter);
        this.ids = ids;
    }

    public List<String> getIds() {
        return ids;
    }

    public void setIds(List<String> ids) {
        this.ids = ids;
    }

    public static AdiEmbeddingSearchRequestBuilder adiBuilder() {
        return new AdiEmbeddingSearchRequestBuilder();
    }

    public static class AdiEmbeddingSearchRequestBuilder {
        private List<String> ids;
        private Embedding queryEmbedding;
        private Integer maxResults;
        private Double minScore;
        private Filter filter;

        AdiEmbeddingSearchRequestBuilder() {
        }

        public AdiEmbeddingSearchRequestBuilder ids(List<String> ids) {
            this.ids = ids;
            return this;
        }

        public AdiEmbeddingSearchRequestBuilder queryEmbedding(Embedding queryEmbedding) {
            this.queryEmbedding = queryEmbedding;
            return this;
        }

        public AdiEmbeddingSearchRequestBuilder maxResults(Integer maxResults) {
            this.maxResults = maxResults;
            return this;
        }

        public AdiEmbeddingSearchRequestBuilder minScore(Double minScore) {
            this.minScore = minScore;
            return this;
        }

        public AdiEmbeddingSearchRequestBuilder filter(Filter filter) {
            this.filter = filter;
            return this;
        }

        public AdiEmbeddingSearchRequest build() {
            return new AdiEmbeddingSearchRequest(this.ids, this.queryEmbedding, this.maxResults, this.minScore, this.filter);
        }

        public String toString() {
            return "AdiEmbeddingSearchRequest.AdiEmbeddingSearchRequestBuilder(queryEmbedding=" + this.queryEmbedding + ", maxResults=" + this.maxResults + ", minScore=" + this.minScore + ", filter=" + this.filter + ")";
        }
    }
}