package com.moyz.adi.common.memory.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

/**
 * Batched result of memory analysis. The LLM returns one entry per fact, allowing
 * a single call to cover all facts and removing the N+1 LLM call pattern.
 * <p>
 * 批量记忆分析结果，每个 fact 对应一条 action，单次 LLM 调用覆盖所有 fact，消除 N+1 问题。
 */
@Data
public class BatchActionMemories {

    private List<BatchActionMemory> actions;

    @Data
    public static class BatchActionMemory {
        /**
         * 原始 fact 文本，用于回溯定位 old memory 上下文。
         */
        private String fact;
        /**
         * 操作目标的旧记忆 id（UPDATE/DELETE 必填，ADD/NONE 可空）。
         */
        private String id;
        /**
         * 最终记忆文本。
         */
        private String text;
        /**
         * ADD / UPDATE / DELETE / NONE。
         */
        private String event;
        @JsonProperty("old_memory")
        private String oldMemory;
    }
}
