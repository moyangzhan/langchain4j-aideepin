package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.SegmentModeEnum;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
public class KbDocumentEditReq {

    private Long id;

    @Min(1)
    private Long kbId;

    private String kbUuid;

    private String uuid;

    @NotBlank
    private String title;

    private String brief;

    /**
     * qa 模式文档可不填（问答数据在段表中）；text/parent_child 模式为切段来源，必填
     */
    private String remark;

    /**
     * 分段模式；为空时按 text 处理
     */
    private SegmentModeEnum segmentMode;

    /**
     * 父子模式子块最大token数；仅父子模式生效，为空时用默认值
     */
    private Integer childMaxChunkSize;

    /**
     * 保存为 qa 模式时由 AI 从正文自动生成问答对；仅对无段行的空 qa 文档生效
     */
    private Boolean autoGenerateQa;
}
