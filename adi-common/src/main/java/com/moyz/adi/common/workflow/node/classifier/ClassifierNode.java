package com.moyz.adi.common.workflow.node.classifier;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataTextContent;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Optional;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 【节点】LLM问题分类器 <br/>
 * 节点内容固定格式：ClassifierNodeConfig
 */
@Slf4j
public class ClassifierNode extends AbstractWfNode {

    public ClassifierNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    protected NodeProcessResult onProcess() {
        ClassifierNodeConfig nodeConfig = checkAndGetConfig(ClassifierNodeConfig.class);
        if (nodeConfig.getCategories().size() < 2) {
            log.warn("问题分类器设置的分类过少,uuid:{},title:{}", node.getUuid(), node.getTitle());
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        //处理类别描述中的变量
        for (ClassifierCategory classifierCategory : nodeConfig.getCategories()) {
            String description = classifierCategory.getCategoryName();
            if (StringUtils.isBlank(description)) {
                continue;
            }
            description = WorkflowUtil.renderTemplate(description, state.getInputs());
            classifierCategory.setCategoryName(description);
        }
        Optional<NodeIOData> defaultInputOpt = state.getDefaultInput();
        if (defaultInputOpt.isEmpty()) {
            throw new BaseException(A_WF_INPUT_INVALID);
        }
        String prompt = ClassifierPrompt.createPrompt(defaultInputOpt.get().valueToString(), nodeConfig.getCategories());
        NodeIOData nodeIODataText = WorkflowUtil.invokeLLM(wfState, nodeConfig.getModelName(), prompt);
        ClassifierLLMResp classifierLLMResp = JsonUtil.fromJson(nodeIODataText.valueToString(), ClassifierLLMResp.class);
        if (null == classifierLLMResp || StringUtils.isBlank(classifierLLMResp.getCategoryUuid())) {
            throw new BaseException(C_LLM_RESPONSE_INVALID);
        }
        String categoryUuid = classifierLLMResp.getCategoryUuid();
        Optional<ClassifierCategory> catOptional = nodeConfig.getCategories()
                .stream()
                .filter(item -> item.getCategoryUuid().equals(categoryUuid))
                .findFirst();
        if (catOptional.isEmpty()) {
            log.error("Can not find the classifier category,uuid:{}", categoryUuid);
            throw new BaseException(C_LLM_RESPONSE_INVALID);
        }
        NodeIODataTextContent datContent = new NodeIODataTextContent();
        datContent.setValue(classifierLLMResp.getCategoryName());
        datContent.setTitle("default");
        List<NodeIOData> result = List.of(NodeIOData.builder().name(DEFAULT_OUTPUT_PARAM_NAME).content(datContent).build());
        String nextNode = catOptional.get().getTargetNodeUuid();
        return NodeProcessResult.builder().nextNodeUuid(nextNode).content(result).build();
    }
}
