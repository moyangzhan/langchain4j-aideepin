package com.moyz.adi.common.workflow;

import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.bsc.langgraph4j.state.AgentState;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_INPUT_PARAM_NAME;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.NODE_PROCESS_STATUS_READY;

/**
 * 工作流节点实例状态 | workflow node instance state
 */
@Setter
@Getter
@ToString(callSuper = true)
public class WfNodeState extends AgentState implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private String uuid = UuidUtil.createShort();
    private int processStatus = NODE_PROCESS_STATUS_READY;
    private String processStatusRemark = "";
    private List<NodeIOData> inputs = new ArrayList<>();
    private List<NodeIOData> outputs = new ArrayList<>();

    /**
     * Constructs an AgentState with the given initial data.
     *
     * @param initData the initial data for the agent state
     */
    public WfNodeState(Map<String, Object> initData) {
        super(initData);
    }

    public WfNodeState() {
        super(Map.of());
    }

    public Optional<NodeIOData> getDefaultInput() {
        return inputs.stream().filter(item -> DEFAULT_INPUT_PARAM_NAME.equals(item.getName())).findFirst();
    }
}
