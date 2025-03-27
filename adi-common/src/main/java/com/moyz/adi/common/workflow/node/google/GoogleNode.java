package com.moyz.adi.common.workflow.node.google;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SearchReturn;
import com.moyz.adi.common.dto.SearchReturnWebPage;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.searchengine.SearchEngineServiceContext;
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
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 以摘要模式进行搜索
 */
@Slf4j
public class GoogleNode extends AbstractWfNode {

    public GoogleNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    protected NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        GoogleNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, GoogleNodeConfig.class);
        if (null == nodeConfigObj) {
            log.warn("找不到Google搜索节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        log.info("GoogleNode config:{}", nodeConfigObj);
        String query;
        if (StringUtils.isNotBlank(nodeConfigObj.getQuery())) {
            query = WorkflowUtil.renderTemplate(nodeConfigObj.getQuery(), state.getInputs());
        } else {
            query = getFirstInputText();
        }
        if (StringUtils.isBlank(query)) {
            log.error("搜索字不能为空");
            throw new BaseException(A_SEARCH_QUERY_IS_EMPTY);
        }
        log.info("GoogleNode query:{}", query);
        SearchReturn searchResult = SearchEngineServiceContext.getService(AdiConstant.SearchEngineName.GOOGLE).search(query, nodeConfigObj.getCountry(), nodeConfigObj.getLanguage(), nodeConfigObj.getTopN());
        if (StringUtils.isNotBlank(searchResult.getErrorMessage())) {
            log.error("Google search error:{}", searchResult.getErrorMessage());
        }
        StringBuilder respText = new StringBuilder();
        for (SearchReturnWebPage searchReturn : searchResult.getItems()) {
            respText.append(searchReturn.getSnippet());
        }
        return NodeProcessResult
                .builder()
                .content(List.of(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", respText.toString())))
                .build();
    }
}
