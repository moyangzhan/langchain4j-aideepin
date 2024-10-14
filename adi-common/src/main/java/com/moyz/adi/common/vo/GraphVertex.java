package com.moyz.adi.common.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GraphVertex {

    private Long id;
    //Apache AGE暂时不支持多标签
    private String label;
    private String name;

    /**
     * 如对应的文本段id
     */
    private String textSegmentId;
    private String description;

    /**
     * 如 kb_uuid=>123,kb_item_uuids=>['123456',['22222']
     */
    private Map<String, Object> metadata;
}
