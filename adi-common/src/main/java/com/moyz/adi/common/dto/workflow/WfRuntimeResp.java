package com.moyz.adi.common.dto.workflow;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class WfRuntimeResp {
    private Long id;
    private String uuid;
    private Long workflowId;
    private ObjectNode input;
    private ObjectNode output;
    private Integer status;
    private String statusRemark;

    private String workflowUuid;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime createTime;
}
