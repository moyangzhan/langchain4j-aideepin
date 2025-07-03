package com.moyz.adi.common.workflow.node.dalle3;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.service.languagemodel.AbstractImageModelService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.WorkflowUtil;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import com.moyz.adi.common.workflow.node.DrawNodeUtil;
import dev.langchain4j.model.openai.OpenAiImageModelName;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 【节点】Dalle3生成图片 <br/>
 */
@Slf4j
public class Dalle3Node extends AbstractWfNode {

    public Dalle3Node(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        Dalle3NodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, Dalle3NodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getPrompt())) {
            log.warn("找不到Dall3节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("Dalle3Node config:{}", nodeConfigObj);
        String prompt;
        if (StringUtils.isNotBlank(nodeConfigObj.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        } else {
            prompt = getFirstInputText();
        }
        log.info("Dalle3Node prompt:{}", prompt);
        if (StringUtils.isBlank(prompt)) {
            log.warn("找不到Dall3节点的提示词");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        AbstractImageModelService<?> imageModelService = ImageModelContext.getModelService(OpenAiImageModelName.DALL_E_3.toString());
        if (null == imageModelService) {
            log.error("image model not found:{}", OpenAiImageModelName.DALL_E_3);
            throw new BaseException(A_MODEL_NOT_FOUND);
        }
        Draw draw = new Draw();
        draw.setGenerateNumber(1);
        draw.setPrompt(prompt);
        draw.setGenerateSize(nodeConfigObj.getSize());
        draw.setGenerateQuality(nodeConfigObj.getQuality());
        draw.setAiModelName(OpenAiImageModelName.DALL_E_3.toString());
        return DrawNodeUtil.createResultContent(wfState.getUser(), draw, imageModelService);
    }
}
