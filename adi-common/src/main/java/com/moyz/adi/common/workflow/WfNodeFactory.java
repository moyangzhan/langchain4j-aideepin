package com.moyz.adi.common.workflow;

import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import com.moyz.adi.common.workflow.node.EndNode;
import com.moyz.adi.common.workflow.node.answer.LLMAnswerNode;
import com.moyz.adi.common.workflow.node.classifier.ClassifierNode;
import com.moyz.adi.common.workflow.node.dalle3.Dalle3Node;
import com.moyz.adi.common.workflow.node.documentextractor.DocumentExtractorNode;
import com.moyz.adi.common.workflow.node.faqextractor.FaqExtractorNode;
import com.moyz.adi.common.workflow.node.google.GoogleNode;
import com.moyz.adi.common.workflow.node.httprequest.HttpRequestNode;
import com.moyz.adi.common.workflow.node.humanfeedback.HumanFeedbackNode;
import com.moyz.adi.common.workflow.node.keywordextractor.KeywordExtractorNode;
import com.moyz.adi.common.workflow.node.knowledgeretrieval.KnowledgeRetrievalNode;
import com.moyz.adi.common.workflow.node.mailsender.MailSendNode;
import com.moyz.adi.common.workflow.node.start.StartNode;
import com.moyz.adi.common.workflow.node.switcher.SwitcherNode;
import com.moyz.adi.common.workflow.node.template.TemplateNode;
import com.moyz.adi.common.workflow.node.tongyiwanx.TongyiwanxNode;

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
            case GOOGLE_SEARCH:
                wfNode = new GoogleNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case DALLE3:
                wfNode = new Dalle3Node(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case TONGYI_WANX:
                wfNode = new TongyiwanxNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case HUMAN_FEEDBACK:
                wfNode = new HumanFeedbackNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case MAIL_SEND:
                wfNode = new MailSendNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case HTTP_REQUEST:
                wfNode = new HttpRequestNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            case END:
                wfNode = new EndNode(wfComponent, nodeDefinition, wfState, nodeState);
                break;
            default:
        }
        return wfNode;
    }
}
