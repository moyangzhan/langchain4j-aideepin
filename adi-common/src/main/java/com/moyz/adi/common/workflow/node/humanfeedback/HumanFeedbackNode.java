package com.moyz.adi.common.workflow.node.humanfeedback;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.HUMAN_FEEDBACK_KEY;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_NOT_FOUND;

/**
 * 人机交互节点
 */
@Slf4j
public class HumanFeedbackNode extends AbstractWfNode {

    public HumanFeedbackNode(WorkflowComponent wfComponent, WorkflowNode node, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, node, wfState, nodeState);
    }

    @Override
    protected NodeProcessResult onProcess() {
        ObjectNode configObj = node.getNodeConfig();
        if (configObj.isEmpty()) {
            log.error("HumanFeedbackNode config is empty");
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        log.info("HumanFeedbackNode config:{}", configObj);
        HumanFeedbackNodeConfig nodeConfig = JsonUtil.fromJson(configObj, HumanFeedbackNodeConfig.class);
        if (null == nodeConfig) {
            log.warn("找不到人机交互节点的配置,{}", state.getUuid());
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        String userInput = state.data().get(HUMAN_FEEDBACK_KEY).toString();
        log.info("用户输入: {}", userInput);
        List<NodeIOData> result = List.of(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "default", userInput));
        return NodeProcessResult.builder().content(result).build();
    }

    public static String getTip(WorkflowNode feedbackNode) {
        ObjectNode configObj = feedbackNode.getNodeConfig();
        if (null == configObj) {
            return "";
        }
        HumanFeedbackNodeConfig nodeConfig = JsonUtil.fromJson(configObj, HumanFeedbackNodeConfig.class);
        if (null == nodeConfig) {
            return "";
        }
        return nodeConfig.getTip();
    }
}