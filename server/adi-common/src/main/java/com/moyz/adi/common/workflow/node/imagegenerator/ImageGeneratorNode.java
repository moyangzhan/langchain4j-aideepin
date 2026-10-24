package com.moyz.adi.common.workflow.node.imagegenerator;

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
import com.moyz.adi.common.workflow.metrics.ImageMetrics;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 【节点】图片生成 <br/>
 */
@Slf4j
public class ImageGeneratorNode extends AbstractWfNode {

    public ImageGeneratorNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
        state.setMetrics(new ImageMetrics());
    }

    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        ImageGeneratorNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, ImageGeneratorNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getModelName())) {
            log.warn("ImageGenerator node configuration missing or incorrect");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("ImageGeneratorNode config:{}", nodeConfigObj);
        String prompt;
        if (StringUtils.isNotBlank(nodeConfigObj.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        } else {
            prompt = getFirstInputText();
        }
        log.info("ImageGeneratorNode prompt:{}", prompt);
        if (StringUtils.isBlank(prompt)) {
            log.warn("ImageGenerator node prompt not found");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        AbstractImageModelService imageModelService = ImageModelContext.getOrDefault(nodeConfigObj.getModelName());
        //记录图片生成指标 | Record image generation metrics
        ImageMetrics imageMetrics = (ImageMetrics) state.getMetrics();
        imageMetrics.setImageModelName(nodeConfigObj.getModelName());
        imageMetrics.setImageSize(nodeConfigObj.getSize());
        if (null == imageModelService) {
            log.error("image service not found,model:{}", nodeConfigObj.getModelName());
            throw new BaseException(A_MODEL_NOT_FOUND);
        }
        Draw draw = new Draw();
        draw.setGenerateNumber(1);
        draw.setPrompt(prompt);
        draw.setGenerateSize(nodeConfigObj.getSize());
        draw.setGenerateSeed(nodeConfigObj.getSeed());
        draw.setGenerateQuality(nodeConfigObj.getQuality());
        draw.setAiModelName(nodeConfigObj.getModelName());
        return DrawNodeUtil.createResultContent(wfState.getUser(), draw, imageModelService);
    }
}
