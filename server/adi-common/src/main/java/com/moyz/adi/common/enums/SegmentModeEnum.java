package com.moyz.adi.common.enums;

import com.baomidou.mybatisplus.annotation.EnumValue;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * 文档分段模式 | Document segment mode
 * <p>
 * 落库、出 API 都用 value（字符串，存于 adi_document.segment_mode 列）。
 * The string value is used both for persistence and for the API.
 * <ul>
 *     <li>text: 通用分段；adi_document_segment 行即分段文本，行自身被向量化</li>
 *     <li>qa: 问答模式；adi_document_segment 行为答案（不向量化），
 *     问题存于 adi_document_segment_question 并被向量化，多问题可关联同一答案</li>
 *     <li>parent_child: 父子分段；adi_document_segment 行为父段（不向量化），
 *     子块存于 adi_document_segment_child_chunk 并被向量化</li>
 * </ul>
 */
@Getter
public enum SegmentModeEnum {

    TEXT("text"),
    QA("qa"),
    PARENT_CHILD("parent_child");

    @EnumValue
    @JsonValue
    private final String value;

    SegmentModeEnum(String value) {
        this.value = value;
    }

    /**
     * 按字符串 value 解析，大小写不敏感；无法识别时返回 null（交由上层校验报错）
     */
    @JsonCreator
    public static SegmentModeEnum fromValue(String value) {
        if (value == null) {
            return null;
        }
        for (SegmentModeEnum mode : values()) {
            if (mode.value.equalsIgnoreCase(value.trim())) {
                return mode;
            }
        }
        return null;
    }
}
