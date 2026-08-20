package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.model.chat.ChatModel;
import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * 图谱化入库参数。图谱复用 document_segment 段行（切段已显式化），
 * 不再自带切分器，也不写 adi_knowledge_base_graph_segment。
 */
@Data
@Builder
public class GraphIngestParam {
    private User user;
    /**
     * 待图谱化的主表段列表（text 段 / qa 答案 / parent_child 父段）
     */
    private List<DocumentSegment> segments;
    private ChatModel ChatModel;
    private List<String> identifyColumns;
    private List<String> appendColumns;
    private boolean isFreeToken;
}
