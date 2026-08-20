package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 文档图谱页（纯账本聚合）返回的边：端点为规范化排序后的实体名对，
 * 描述为片段拼接、权重为片段强度求和——全部读时聚合派生。
 */
@Data
public class KbEdgeDto {

    private String sourceName;

    private String targetName;

    private String description;

    private Double weight;
}
