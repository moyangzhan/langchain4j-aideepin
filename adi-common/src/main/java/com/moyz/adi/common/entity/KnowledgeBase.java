package com.moyz.adi.common.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@TableName("adi_knowledge_base")
@Schema(title = "知识库实体", description = "知识库表")
public class KnowledgeBase extends BaseEntity {

    @Schema(title = "uuid")
    @TableField("uuid")
    private String uuid;

    @Schema(title = "名称")
    @TableField("title")
    private String title;

    @Schema(title = "描述")
    @TableField("remark")
    private String remark;

    @Schema(title = "是否公开")
    @TableField("is_public")
    private Boolean isPublic;

    @Schema(title = "点赞数")
    @TableField("star_count")
    private Long starCount;

    @Schema(title = "所属人id")
    @TableField("owner_id")
    private Long ownerId;

    @Schema(title = "所属人名称")
    @TableField("owner_name")
    private String ownerName;
}
