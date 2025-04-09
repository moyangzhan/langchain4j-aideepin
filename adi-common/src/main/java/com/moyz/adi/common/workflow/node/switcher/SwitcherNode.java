package com.moyz.adi.common.workflow.node.switcher;

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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_NOT_FOUND;
import static com.moyz.adi.common.workflow.WfNodeIODataUtil.changeInputsToOutputs;

/**
 * 【节点】条件分支
 */
@Slf4j
public class SwitcherNode extends AbstractWfNode {

    public SwitcherNode(WorkflowComponent wfComponent, WorkflowNode node, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, node, wfState, nodeState);
    }

    @Override
    protected NodeProcessResult onProcess() {
        ObjectNode nodeConfigObj = node.getNodeConfig();
        if (nodeConfigObj.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        SwitcherNodeConfig nodeConfig = JsonUtil.fromJson(nodeConfigObj, SwitcherNodeConfig.class);
        if (null == nodeConfig || CollectionUtils.isEmpty(nodeConfig.getCases())) {
            log.warn("找不到条件分支节点的配置,uuid:{},title:{}", node.getUuid(), node.getTitle());
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        String nextNode = nodeConfig.getDefaultTargetNodeUuid();
        if (StringUtils.isBlank(nextNode)) {
            log.error("Switcher default downstream is empty,node:{},title:{}", node.getUuid(), node.getTitle());
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        //初始化配置的各种case
        for (SwitcherCase switcherCase : nodeConfig.getCases()) {
            List<SwitcherCase.Condition> conditions = switcherCase.getConditions();
            if (StringUtils.isAnyBlank(switcherCase.getTargetNodeUuid(), switcherCase.getOperator()) || CollectionUtils.isEmpty(conditions)) {
                log.warn("Switcher case error:{}", switcherCase);
                continue;
            }
            int conditionPassCount = 0;
            boolean casePass = false;
            boolean allConditionPassRequired = switcherCase.getOperator().equals(LogicOperatorEnum.AND.getName());
            for (SwitcherCase.Condition condition : switcherCase.getConditions()) {
                NodeIOData ioData = createByReferParam(condition.getNodeUuid(), condition.getNodeParamName());
                if (null == ioData || null == ioData.getContent()) {
                    log.warn("Switcher找不到引用的节点参数,nodeUuid:{},paramName:{}", condition.getNodeUuid(), condition.getNodeParamName());
                    continue;
                }
                String inputValue = ioData.valueToString().toLowerCase();
                String value = condition.getValue().toLowerCase();
                boolean conditionPass = processCondition(value, inputValue, condition.getOperator());
                if (conditionPass) {
                    conditionPassCount++;
                }
                if ((conditionPassCount == conditions.size()) || (conditionPassCount > 0 && !allConditionPassRequired)) {
                    casePass = true;
                    break;
                }
            }
            if (casePass) {
                nextNode = switcherCase.getTargetNodeUuid();
                break;
            }
        }
        if (StringUtils.isBlank(nextNode)) {
            log.error("Switcher downstream is empty,node:{},title:{}", node.getUuid(), node.getTitle());
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        return NodeProcessResult.builder().nextNodeUuid(nextNode).content(changeInputsToOutputs(state.getInputs())).build();
    }

    private boolean processCondition(String defValue, String value, String operator) {
        boolean conditionPass = false;
        switch (OperatorEnum.getByName(operator)) {
            case CONTAINS:
                conditionPass = defValue.contains(value);
                break;
            case NOT_CONTAINS:
                conditionPass = !defValue.contains(value);
                break;
            case START_WITH:
                conditionPass = defValue.startsWith(value);
                break;
            case END_WITH:
                conditionPass = defValue.endsWith(value);
                break;
            case EMPTY:
                conditionPass = StringUtils.isBlank(defValue);
                break;
            case NOT_EMPTY:
                conditionPass = StringUtils.isNotBlank(defValue);
                break;
            case EQUAL:
                conditionPass = defValue.equals(value);
                break;
            case NOT_EQUAL:
                conditionPass = !defValue.equals(value);
                break;
            case GREATER:
                try {
                    double in = Double.parseDouble(defValue);
                    double vl = Double.parseDouble(value);
                    conditionPass = in > vl;
                } catch (Exception e) {
                    log.error("parse double error", e);
                }
                break;
            case GREATER_OR_EQUAL:
                try {
                    double in = Double.parseDouble(defValue);
                    double vl = Double.parseDouble(value);
                    conditionPass = in >= vl;
                } catch (Exception e) {
                    log.error("parse double error", e);
                }
                break;
            case LESS:
                try {
                    double in = Double.parseDouble(defValue);
                    double vl = Double.parseDouble(value);
                    conditionPass = in < vl;
                } catch (Exception e) {
                    log.error("parse double error", e);
                }
                break;
            case LESS_OR_EQUAL:
                try {
                    double in = Double.parseDouble(defValue);
                    double vl = Double.parseDouble(value);
                    conditionPass = in <= vl;
                } catch (Exception e) {
                    log.error("parse double error", e);
                }
                break;
        }
        return conditionPass;
    }
}
