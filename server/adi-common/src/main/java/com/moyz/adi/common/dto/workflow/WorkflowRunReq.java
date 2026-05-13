package com.moyz.adi.common.dto.workflow;

import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class WorkflowRunReq {
    private List<ObjectNode> inputs;
}
