package com.moyz.adi.common.memory.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
public class ActionMemories {

    private List<ActionMemory> memory;

    @Data
    public static class ActionMemory {
        private String id;
        private String text;
        private String event;
        @JsonProperty("old_memory")
        private String oldMemory;
    }
}
