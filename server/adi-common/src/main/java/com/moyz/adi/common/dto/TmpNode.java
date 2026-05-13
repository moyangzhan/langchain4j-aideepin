package com.moyz.adi.common.dto;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.workflow.WfNodeInputConfig;
import lombok.Data;

@Data
public class TmpNode {
    private Long id;
    private String oldUuid;
    private String newUuid;
    private Long componentId;
    private WfNodeInputConfig inputConfig;
    private ObjectNode nodeConfig;
}
