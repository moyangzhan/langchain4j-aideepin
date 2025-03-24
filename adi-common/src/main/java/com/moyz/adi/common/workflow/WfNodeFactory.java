package com.moyz.adi.common.workflow;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import com.moyz.adi.common.workflow.node.EndNode;
import com.moyz.adi.common.workflow.node.faqextractor.FaqExtractorNode;
import com.moyz.adi.common.workflow.node.knowledgeretrieval.KnowledgeRetrievalNode;
import com.moyz.adi.common.workflow.node.start.StartNode;
import com.moyz.adi.common.workflow.node.answer.LLMAnswerNode;
import com.moyz.adi.common.workflow.node.classifier.ClassifierNode;
import com.moyz.adi.common.workflow.node.documentextractor.DocumentExtractorNode;
import com.moyz.adi.common.workflow.node.keywordextractor.KeywordExtractorNode;
import com.moyz.adi.common.workflow.node.switcher.SwitcherNode;
import com.moyz.adi.common.workflow.node.template.TemplateNode;

public class WfNodeFactory {
    public static AbstractWfNode create(WorkflowComponent wfComponent, WorkflowNode nodeDefinition, WfState wfState, WfNodeState nodeState) {
        AbstractWfNode wfNode = null;
        switch (WfComponentNameEnum.getByName(wfComponent.getName())) {
            case START:
                wfNode = new StartNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case LLM_ANSWER:
                wfNode = new LLMAnswerNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case CLASSIFIER:
                wfNode = new ClassifierNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case SWITCHER:
                wfNode = new SwitcherNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case TEMPLATE:
                wfNode = new TemplateNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case KEYWORD_EXTRACTOR:
                wfNode = new KeywordExtractorNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case DOCUMENT_EXTRACTOR:
                wfNode = new DocumentExtractorNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case FAQ_EXTRACTOR:
                wfNode = new FaqExtractorNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case KNOWLEDGE_RETRIEVER:
                wfNode = new KnowledgeRetrievalNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case END:
                wfNode = new EndNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            default:
        }
        return wfNode;
    }
}
