package com.moyz.adi.common.workflow.node.knowledgeretrieval;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.rag.EmbeddingRAG;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import dev.langchain4j.rag.content.Content;
import dev.langchain4j.rag.content.retriever.ContentRetriever;
import dev.langchain4j.rag.query.Query;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;
import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 【节点】知识抽取 <br/>
 * 节点内容固定格式：KnowledgeRetrievalNodeConfig
 */
@Slf4j
public class KnowledgeRetrievalNode extends AbstractWfNode {

    public KnowledgeRetrievalNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    /**
     * nodeConfig格式：<br/>
     * {"knowledge_base_uuid": "","score":0.6,"top_n":3,"is_strict": false, "default_response":"数据不存在~~~"}<br/>
     */
    @Override
    public NodeProcessResult onProcess() {
        ObjectNode objectConfig = node.getNodeConfig();
        if (objectConfig.isEmpty()) {
            throw new BaseException(A_WF_NODE_CONFIG_NOT_FOUND);
        }
        KnowledgeRetrievalNodeConfig nodeConfigObj = JsonUtil.fromJson(objectConfig, KnowledgeRetrievalNodeConfig.class);
        if (null == nodeConfigObj || StringUtils.isBlank(nodeConfigObj.getKnowledgeBaseUuid())) {
            log.warn("找不到知识检索节点的配置");
            throw new BaseException(A_WF_NODE_CONFIG_ERROR);
        }
        String kbUuid = nodeConfigObj.getKnowledgeBaseUuid();
        log.info("KnowledgeRetrievalNode config:{}", nodeConfigObj);
        String textInput = getFirstInputText();
        if (StringUtils.isBlank(textInput)) {
            log.warn("输入内容为空");
            return NodeProcessResult
                    .builder()
                    .content(List.of(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", "")))
                    .build();
        }
        EmbeddingRAG embeddingRAG = SpringUtil.getBean(EmbeddingRAG.class);
        Map<String, String> metadataCond = Map.of(AdiConstant.MetadataKey.KB_UUID, kbUuid);
        ContentRetriever retriever = embeddingRAG.createRetriever(metadataCond, nodeConfigObj.getTopN(), nodeConfigObj.getScore(), nodeConfigObj.getIsStrict());
        StringBuilder resp = new StringBuilder();
        try {
            List<Content> contents = retriever.retrieve(Query.from(textInput));
            for (Content content : contents) {
                resp.append(content.textSegment().text());
            }
        } catch (BaseException e) {
            if (B_BREAK_SEARCH.getCode().equals(e.getCode())) {
                log.warn(B_BREAK_SEARCH.getInfo());
            } else {
                throw e;
            }
        }

        String respText = resp.toString();
        if (StringUtils.isBlank(respText) && StringUtils.isNotBlank(nodeConfigObj.getDefaultResponse())) {
            respText = nodeConfigObj.getDefaultResponse();
        }
        return NodeProcessResult
                .builder()
                .content(List.of(NodeIOData.createByText(DEFAULT_OUTPUT_PARAM_NAME, "", respText)))
                .build();
    }
}
