package com.moyz.adi.common.vo;

import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.User;
import dev.langchain4j.model.chat.ChatModel;
import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.function.Supplier;

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
    /**
     * LLM 调用记录用：来源对象 id（所属文档 id，写 adi_llm_call_record.source_id）
     */
    private Long sourceId;
    /**
     * LLM 调用记录用：模型平台/名称（调用方从 AiModel 取，GraphRag 内不持有 LLM 服务）
     */
    private String modelPlatform;
    private String modelName;
    /**
     * 协作式取消信号（任务队列的版本守卫）：每段抽取前检查，true 即抛
     * IndexTaskCancelledException——被新版本 supersede 的图谱任务不再继续消耗 LLM token
     */
    private Supplier<Boolean> cancelSignal;
}
