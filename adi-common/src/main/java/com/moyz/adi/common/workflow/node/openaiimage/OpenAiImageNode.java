package com.moyz.adi.common.workflow.node.openaiimage;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.languagemodel.AbstractImageModelService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import com.moyz.adi.common.workflow.node.DrawNodeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
public class OpenAiImageNode extends AbstractWfNode {

    public OpenAiImageNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        OpenAiImageNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, OpenAiImageNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getPrompt())) {
            log.warn("找不到OpenAiImage节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("OpenAiImageNode config:{}", nodeConfigObj);
        String prompt;
        if (StringUtils.isNotBlank(nodeConfigObj.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        } else {
            prompt = getFirstInputText();
        }
        log.info("OpenAiImageNode prompt:{}", prompt);
        if (StringUtils.isBlank(prompt)) {
            log.warn("找不到OpenAiImage节点的提示词");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        AbstractImageModelService imageModelService = ImageModelContext.getFirstModelService("openai");
        if (null == imageModelService) {
            log.error("No enabled OpenAI image model found");
            throw new BaseException(A_MODEL_NOT_FOUND);
        }
        String modelName = imageModelService.getAiModel().getName();
        Draw draw = new Draw();
        draw.setGenerateNumber(1);
        draw.setPrompt(prompt);
        draw.setGenerateSize(nodeConfigObj.getSize());
        draw.setGenerateQuality(nodeConfigObj.getQuality());
        draw.setAiModelName(modelName);
        return DrawNodeUtil.createResultContent(wfState.getUser(), draw, imageModelService);
    }
}
