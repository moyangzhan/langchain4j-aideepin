package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 分段列表 DTO（模式感知）：
 * text→content 为段文本；qa→content 为答案、questions 为其问题；
 * parent_child→content 为父段、children 为其子块。
 */
@Data
public class DocumentSegmentDto {

    private Long id;

    private String uuid;

    private String docUuid;

    private Integer position;

    private String content;

    private Integer wordCount;

    private Integer hitCount;

    private Boolean isEnabled;

    private LocalDateTime enabledChangeTime;

    private EmbeddingStatusEnum embeddingStatus;

    private GraphicalStatusEnum graphicalStatus;

    /**
     * True when a backfilled embedding id is missing from the vector store (drift detection);
     * null when no presence checker is available
     */
    private Boolean vectorMissing;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;

    /**
     * qa 模式：该答案下的所有问题
     */
    private List<DocumentSegmentQuestionDto> questions;

    /**
     * parent_child 模式：该父段下的所有子块
     */
    private List<DocumentSegmentChildChunkDto> children;
}
