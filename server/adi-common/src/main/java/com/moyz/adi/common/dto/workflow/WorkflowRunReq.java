package com.moyz.adi.common.dto.workflow;

import tools.jackson.databind.node.ObjectNode;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class WorkflowRunReq {
    private List<ObjectNode> inputs;
}
