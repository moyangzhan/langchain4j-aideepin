package com.moyz.adi.common.workflow.node.answer;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.*;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import dev.langchain4j.data.message.UserMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_NOT_FOUND;

/**
 * 【节点】LLM生成回答 <br/>
 * 节点内容固定格式：LLMAnswerNodeConfig
 */
@Slf4j
public class LLMAnswerNode extends AbstractWfNode {

    public LLMAnswerNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    /**
     * nodeConfig格式：<br/>
     * {"prompt": "将以下内容翻译成英文：{input}","model_name":"deepseek-chat"}<br/>
     *
     * @return LLM的返回内容
     */
    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        LLMAnswerNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, LLMAnswerNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getPrompt())) {
            log.warn("找不到问答节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("LLM answer node config:{}", nodeConfigObj);
        String prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        log.info("LLM prompt:{}", prompt);
        String modelName = nodeConfigObj.getModelName();
        //调用LLM
        WorkflowUtil.streamingInvokeLLM(wfState, state, node, modelName, List.of(UserMessage.from(prompt)));
        return new NodeProcessResult();
    }
}
