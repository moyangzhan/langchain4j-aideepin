package com.moyz.adi.common.workflow.node.tongyiwanx;

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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import static com.moyz.adi.common.cosntant.AdiConstant.ModelPlatform.DASHSCOPE;
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 【节点】通义万相-生成图片 <br/>
 */
@Slf4j
public class TongyiwanxNode extends AbstractWfNode {

    public TongyiwanxNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        TongyiwanxNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, TongyiwanxNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getModelName())) {
            log.warn("通义万相节点的配置不存在或错误");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("TongyiwanxNode config:{}", nodeConfigObj);
        String prompt;
        if (StringUtils.isNotBlank(nodeConfigObj.getPrompt())) {
            prompt = WorkflowUtil.renderTemplate(nodeConfigObj.getPrompt(), state.getInputs());
        } else {
            prompt = getFirstInputText();
        }
        log.info("TongyiwanxNode prompt:{}", prompt);
        if (StringUtils.isBlank(prompt)) {
            log.warn("找不到通义万相节点的提示词");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        AbstractImageModelService<?> imageModelService = ImageModelContext.getModelService(nodeConfigObj.getModelName());
        if (null == imageModelService) {
            log.error("image service not found,ai platform:{}", DASHSCOPE);
            throw new BaseException(A_MODEL_NOT_FOUND);
        }
        Draw draw = new Draw();
        draw.setGenerateNumber(1);
        draw.setPrompt(prompt);
        draw.setGenerateSize(nodeConfigObj.getSize());
        draw.setGenerateSeed(nodeConfigObj.getSeed());
        draw.setAiModelName(nodeConfigObj.getModelName());
        return DrawNodeUtil.createResultContent(wfState.getUser(), draw, imageModelService);
    }
}
