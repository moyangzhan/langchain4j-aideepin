package com.moyz.adi.common.workflow.node;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeIODataUtil;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class StartNode extends AbstractWfNode {

    public StartNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    public NodeProcessResult onProcess() {
        if (state.getInputs().isEmpty()) {
            state.getInputs().addAll(wfState.getInput());
        }
        List<NodeIOData> result = WfNodeIODataUtil.changeInputsToOutputs(state.getInputs());
        return NodeProcessResult.builder().content(result).build();
    }

}
