package com.moyz.adi.common.workflow.node.keywordextractor;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_WF_NODE_CONFIG_NOT_FOUND;

/**
 * 【节点】关键词抽取 <br/>
 * 节点内容固定格式：KeywordExtractorNodeConfig
 */
@Slf4j
public class KeywordExtractorNode extends AbstractWfNode {

    public KeywordExtractorNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    /**
     * nodeConfig格式：<br/>
     * {"top_n": 10,"model_name":"deepseek-chat"}<br/>
     *
     * @return LLM的返回内容
     */
    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        KeywordExtractorNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, KeywordExtractorNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getModelName())) {
            log.warn("找不到关键词提取节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("KeywordExtractorNode config:{}", nodeConfigObj);
        if (state.getInputs().isEmpty()) {
            log.warn("KeywordExtractorNode inputs is empty");
            return new NodeProcessResult();
        }
        String userInput = state.getInputs().get(0).valueToString();
        String prompt = KeywordExtractorPrompt.getPrompt(nodeConfigObj.getTopN(), userInput);
        List<ChatMessage> llmMessages = new ArrayList<>();
        llmMessages.add(UserMessage.from(prompt));
        log.info("KeywordExtractorNode prompt:{}", prompt);

        //调用LLM
        WorkflowUtil.streamingInvokeLLM(wfState, state, node, nodeConfigObj.getModelName(), llmMessages);
        return new NodeProcessResult();
    }
}
