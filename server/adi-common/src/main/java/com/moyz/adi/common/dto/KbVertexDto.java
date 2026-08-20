package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 文档图谱页（纯账本聚合）返回的顶点：以实体名为键，描述为全部贡献片段的拼接。
 * 不携带图库内部 id（跨后端不稳定、重抽即变），渲染与展示对图库零依赖。
 */
@Data
public class KbVertexDto {

    private String name;

    private String description;
}
