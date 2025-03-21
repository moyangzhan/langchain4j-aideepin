package com.moyz.adi.common.workflow.node.documentextractor;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.WorkflowComponent;
import com.moyz.adi.common.entity.WorkflowNode;
import com.moyz.adi.common.enums.WfIODataTypeEnum;
import com.moyz.adi.common.helper.AdiFileHelper;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.workflow.NodeProcessResult;
import com.moyz.adi.common.workflow.WfNodeState;
import com.moyz.adi.common.workflow.WfState;
import com.moyz.adi.common.workflow.data.NodeIOData;
import com.moyz.adi.common.workflow.data.NodeIODataFilesContent;
import com.moyz.adi.common.workflow.data.NodeIODataTextContent;
import com.moyz.adi.common.workflow.node.AbstractWfNode;
import dev.langchain4j.data.document.Document;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.WorkflowConstant.DEFAULT_OUTPUT_PARAM_NAME;

/**
 * 【节点】文档解析 <br/>
 */
@Slf4j
public class DocumentExtractorNode extends AbstractWfNode {

    public DocumentExtractorNode(WorkflowComponent wfComponent, WorkflowNode nodeDef, WfState wfState, WfNodeState nodeState) {
        super(wfComponent, nodeDef, wfState, nodeState);
    }

    @Override
    public NodeProcessResult onProcess() {
        StringBuilder documentText = new StringBuilder();
        List<NodeIOData> list = state.getInputs();
        List<String> fileUuids = new ArrayList<>();
        for (NodeIOData nodeIOData : list) {
            if (WfIODataTypeEnum.FILES.getValue().equals(nodeIOData.getContent().getType())) {
                NodeIODataFilesContent filesContent = (NodeIODataFilesContent) nodeIOData.getContent();
                fileUuids.addAll(filesContent.getValue());
            }
        }
        FileService fileService = SpringUtil.getBean(FileService.class);
        //解析文档
        try {
            for (String uuid : fileUuids) {
                AdiFile adiFile = fileService.getFile(uuid);
                Document document = AdiFileHelper.loadDocument(adiFile);
                if (null == document) {
                    log.warn("{}的文件类型:{}无法解析，忽略", adiFile.getUuid(), adiFile.getExt());
                    continue;
                }
                documentText.append(document.text());
            }
        } catch (Exception e) {
            log.error("解析文档失败", e);
        }
        NodeIODataTextContent dataContent = new NodeIODataTextContent();
        dataContent.setValue(documentText.toString());
        dataContent.setTitle("");
        List<NodeIOData> result = List.of(NodeIOData.builder().name(DEFAULT_OUTPUT_PARAM_NAME).content(dataContent).build());
        return NodeProcessResult.builder().content(result).build();
    }
}
