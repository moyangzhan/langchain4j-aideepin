package com.moyz.adi.common.workflow;

import com.moyz.adi.common.workflow.data.NodeIOData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class NodeProcessResult {

    private List<NodeIOData> content = new ArrayList<>();

    /**
     * 条件执行时使用
     */
    private String nextNodeUuid;
}
