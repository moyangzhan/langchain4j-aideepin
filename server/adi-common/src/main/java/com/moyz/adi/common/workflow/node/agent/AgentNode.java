package com.moyz.adi.common.workflow.node.agent;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.service.AgentService;
import com.moyz.adi.common.service.LocalAgentService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.AgentRequest;
import com.moyz.adi.common.vo.AgentResult;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.metrics.AgentMetrics;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;

/**
 * 【节点】Agent 节点 — 调用角色（Character）的完整能力
 * <p>
 * Agent node — invokes a Character with its full capabilities
 * (system prompt, knowledge base RAG, MCP tools, memory, web search).
 * </p>
 * <p>
 * 节点内容固定格式：{@link AgentNodeConfig}
 * </p>
 */
@Slf4j
public class AgentNode extends AbstractWfNode {

    private final AgentService agentService;

    public AgentNode(WorkflowComponent wfComponent, WorkflowNode nodeDef,
                     WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
        this.agentService = SpringUtil.getBean(AgentService.class);
        state.setMetrics(new AgentMetrics());
    }

    /**
     * nodeConfig 格式：
     * <pre>{@code
     * {
     *   "character_uuid": "xxx",
     *   "model_platform": "deepseek",     // 可选
     *   "model_name": "deepseek-chat",     // 可选
     *   "prompt": "总结：{input}",          // 可选
     *   "enable_rag": true,
     *   "enable_mcp": true,
     *   "enable_web_search": false
     * }
     * }</pre>
     */
    @Override
    public NodeProcessResult onProcess() {
        AgentNodeConfig config = checkAndGetConfig(AgentNodeConfig.class);
        String inputText = getFirstInputText();

        //渲染 prompt 模板（同 LLMAnswerNode 模式）
        String prompt = inputText;
        if (StringUtils.isNotBlank(config.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(config.getPrompt(), state.getInputs());
        }

        log.info("Agent node invoking character:{}, input length:{}", config.getCharacterUuid(), prompt.length());

        //构建通用请求
        AgentRequest request = AgentRequest.builder()
                .characterUuid(config.getCharacterUuid())
                .modelPlatform(config.getModelPlatform())
                .modelName(config.getModelName())
                .inputText(prompt)
                .enableRag(Boolean.TRUE.equals(config.getEnableRag()))
                .enableMcp(!Boolean.FALSE.equals(config.getEnableMcp()))
                .enableWebSearch(Boolean.TRUE.equals(config.getEnableWebSearch()))
                .build();

        //调用 Agent
        AgentResult result = agentService.invoke(request, wfState.getUser(), wfState.getUuid());

        //记录指标
        AgentMetrics metrics = (AgentMetrics) state.getMetrics();
        if (result.getInputTokens() != null) {
            metrics.setInputTokens(result.getInputTokens());
        }
        if (result.getOutputTokens() != null) {
            metrics.setOutputTokens(result.getOutputTokens());
        }
        metrics.setCharacterUuid(config.getCharacterUuid());
        if (result.getRetrievalCount() != null) {
            metrics.setRetrievalCount(result.getRetrievalCount());
        }
        if (config.getModelName() != null) {
            metrics.setModelName(config.getModelName());
        }
        if (config.getModelPlatform() != null) {
            metrics.setModelPlatform(config.getModelPlatform());
        }

        return NodeProcessResult.builder()
                .content(List.of(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", result.getAnswer())))
                .build();
    }
}
