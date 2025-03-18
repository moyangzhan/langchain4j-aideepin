package com.moyz.adi.common.workflow.node.template;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_NOT_FOUND;

@Slf4j
public class TemplateNode extends AbstractWfNode {

    public TemplateNode(WorkflowComponent wfComponent, WorkflowNode node, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, node, wfState, nodeState);
    }

    @Override
    protected NodeProcessResult onProcess() {
        ObjectNode nodeConfigObj = node.getNodeConfig();
        if (nodeConfigObj.isEmpty()) {
            log.error("TemplateNode config is empty");
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        TemplateNodeConfig nodeConfig = JsonUtil.fromJson(nodeConfigObj, TemplateNodeConfig.class);
        if (null == nodeConfig || StringUtils.isBlank(nodeConfig.getTemplate())) {
            log.warn("找不到模板转换节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("Template node config:{}", nodeConfigObj);
        String content = WorkflowUtil.renderTemplate(nodeConfig.getTemplate(), state.getInputs());
        NodeIOData output = NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", content);
        return NodeProcessResult.builder().content(List.of(output)).build();
    }
}
