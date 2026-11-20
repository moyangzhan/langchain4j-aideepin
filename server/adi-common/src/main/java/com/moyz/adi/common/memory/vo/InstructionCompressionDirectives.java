package com.moyz.adi.common.memory.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

/**
 * LLM 压缩指令结果：按 uuid 编辑式（合并保留一行、删除若干行），保住 uuid 结构使引用不断链。
 * <p>
 * LLM compression directives: uuid-keyed edits (merge keeps one row, delete drops
 * rows) so uuid structure survives and references stay intact.
 */
@Data
public class InstructionCompressionDirectives {

    @JsonProperty("merges")
    private List<Merge> merges;

    @JsonProperty("deletes")
    private List<String> deletes;

    @Data
    public static class Merge {
        /**
         * 保留行的规则 uuid（32 位无横线）。 | Kept rule's uuid (32 chars, no dashes).
         */
        private String keep;

        /**
         * 合并后的规则文本。 | Merged rule text.
         */
        private String text;

        /**
         * 被并入而应删除的规则 uuid 列表。 | Rule uuids folded into the kept row.
         */
        private List<String> drop;
    }
}
