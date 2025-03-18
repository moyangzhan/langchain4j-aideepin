package com.moyz.adi.common.workflow.data;

import lombok.Data;

import java.util.List;

@Data
public class WfUserReq {
    private List<NodeIOData> inputs;
}
