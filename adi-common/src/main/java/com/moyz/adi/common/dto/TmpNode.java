package com.moyz.adi.common.dto;

import com.moyz.adi.common.workflow.WfNodeInputConfig;
import lombok.Data;

@Data
public class TmpNode {
    private Long id;
    private String oldUuid;
    private String uuid;
    private WfNodeInputConfig inputConfig;
}
