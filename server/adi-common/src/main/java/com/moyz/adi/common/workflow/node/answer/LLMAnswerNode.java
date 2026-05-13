package com.moyz.adi.common.workflow.node.answer;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import dev.langchain4j.data.message.UserMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

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
     * {"prompt": "将以下内容翻译成英文：{input}","model_platform":"deepseek","model_name":"deepseek-chat"}<br/>
     *
     * @return LLM的返回内容
     */
    @Override
    public NodeProcessResult onProcess() {
        LLMAnswerNodeConfig nodeConfigObj = checkAndGetConfig(LLMAnswerNodeConfig.class);
        String inputText = getFirstInputText();
        log.info("LLM answer node config:{}", nodeConfigObj);
        String prompt = inputText;
        if (StringUtils.isNotBlank(nodeConfigObj.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        }
        log.info("LLM prompt:{}", prompt);
        String modelName = nodeConfigObj.getModelName();
        //调用LLM
        WorkflowUtil.streamingInvokeLLM(wfState, state, node, nodeConfigObj.getModelPlatform(), modelName, List.of(UserMessage.from(prompt)));
        return new NodeProcessResult();
    }
}
