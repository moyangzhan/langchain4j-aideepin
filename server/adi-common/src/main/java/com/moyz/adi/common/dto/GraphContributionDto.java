package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 账本清理用：某段（或某文档）贡献的元素键 + 该元素的"其他贡献者数"（0 = 独占，可安全删除）。
 * 顶点仅填 name；边填 sourceName/targetName。
 */
@Data
public class GraphContributionDto {

    private String name;

    private String targetName;

    private Integer otherContributors;
}
