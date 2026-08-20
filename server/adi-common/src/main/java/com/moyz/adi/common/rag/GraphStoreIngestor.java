package com.moyz.adi.common.rag;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentGraphEdge;
import com.moyz.adi.common.entity.DocumentGraphVertex;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.DocumentGraphProvenanceService;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.*;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.store.embedding.filter.Filter;
import dev.langchain4j.store.embedding.filter.comparison.IsEqualTo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.tuple.Triple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.MAX_METADATA_VALUE_LENGTH;

/**
 * 图谱存储入库器：把「段抽取结果」写入图数据库，并同步维护段溯源账本（双写）。
 * <p>
 * 切段职责已上移到 SegmentIndexService（document_segment 为唯一事实源），
 * 本类负责：LLM 已抽取出的实体/关系 → 顶点/边的合并写入 + 账本贡献行落库。
 * 每个元素为 Triple(段TextSegment, 段uuid即textSegmentId, LLM抽取响应)。
 * 图库上的合并 description/weight 与 text_segment_id 串退化为检索缓存；
 * 操作与展示语义（独占判定、描述聚合、停用清理）全部以账本为准。
 */
@Builder
@AllArgsConstructor
@Slf4j
public class GraphStoreIngestor {

    private final GraphStore graphStore;

    /**
     * 查询时 where 语句的条件字段名
     */
    private final List<String> identifyColumns;

    /**
     * 更新时 Set 语句的追加字段名，值为数组类型<br/>
     * 如已存在数据kb_item_uuids=['ab']，则更新时对该字段追加新数据,最终结果为kb_item_uuids=['ab','cd']
     */
    private final List<String> appendColumns;

    /**
     * 把图谱抽取结果写入图数据库。extracted 为空时直接返回。
     */
    public void ingestExtracted(List<Triple<TextSegment, String, String>> extracted) {
        log.info("Starting to store {} extracted segments into the graph store", extracted.size());
        // 账本行攒批（键内去重、富信息行优先），循环结束后一次落库
        Map<String, DocumentGraphVertex> vertexRows = new LinkedHashMap<>();
        Map<String, DocumentGraphEdge> edgeRows = new LinkedHashMap<>();
        for (Triple<TextSegment, String, String> triple : extracted) {
            TextSegment segment = triple.getLeft();
            String textSegmentId = triple.getMiddle();
            String response = triple.getRight();
            Map<String, Object> metadata = segment.metadata().toMap();
            String kbUuid = String.valueOf(metadata.get(AdiConstant.MetadataKey.KB_UUID));
            String docUuid = String.valueOf(metadata.get(AdiConstant.MetadataKey.KB_ITEM_UUID));
            log.info("Graph response:{}", response);
            if (StringUtils.isBlank(response)) {
                log.warn("Response is empty, segmentId:{}", textSegmentId);
                continue;
            }

            Filter filter = null;
            for (Map.Entry<String, Object> entry : metadata.entrySet()) {
                boolean contain = identifyColumns.contains(entry.getKey());
                if (contain) {
                    if (null == filter) {
                        filter = new IsEqualTo(entry.getKey(), entry.getValue());
                    } else {
                        filter = filter.and(new IsEqualTo(entry.getKey(), entry.getValue()));
                    }
                }
            }
            if (null == filter) {
                throw new BaseException(ErrorEnum.B_GRAPH_FILTER_NOT_FOUND);
            }

            String[] rows = StringUtils.split(response, AdiConstant.GRAPH_RECORD_DELIMITER);
            for (String row : rows) {
                String graphRow = row;
                graphRow = graphRow.replaceAll("^\\(|\\)$", "");
                String[] recordAttributes = StringUtils.split(graphRow, AdiConstant.GRAPH_TUPLE_DELIMITER);
                if (recordAttributes.length >= 4 && (recordAttributes[0].contains("\"entity\"") || recordAttributes[0].contains("\"实体\""))) {
                    String entityName = AdiStringUtil.clearStr(recordAttributes[1].toUpperCase());
                    String entityType = AdiStringUtil.clearStr(recordAttributes[2].toUpperCase()).replaceAll("[^a-zA-Z0-9\\s\\u4E00-\\u9FA5]+", "").replace(" ", "");
                    String entityDescription = AdiStringUtil.clearStr(recordAttributes[3]);
                    log.info("entityName:{},entityType:{},entityDescription:{}", entityName, entityType, entityDescription);
                    addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, entityName, entityType, entityDescription));
                    //实体如果不存在图数据库中，插入一个新的实体，否则追加textSegmentId、description以及metadata中指定的内容
                    List<GraphVertex> existVertices = graphStore.searchVertices(
                            GraphVertexSearch.builder()
                                    .label(entityType)
                                    .limit(1)
                                    .names(List.of(entityName))
                                    .metadataFilter(filter)
                                    .build()
                    );
                    if (CollectionUtils.isNotEmpty(existVertices)) {
                        GraphVertex existVertex = existVertices.get(0);
                        String newTextSegmentId = existVertex.getTextSegmentId() + "," + textSegmentId;
                        String newDesc = existVertex.getDescription() + "\n" + entityDescription;

                        appendExistsToNewOne(existVertex.getMetadata(), metadata);
                        GraphVertex newData = GraphVertex.builder().textSegmentId(newTextSegmentId).description(newDesc).metadata(metadata).build();
                        graphStore.updateVertex(
                                GraphVertexUpdateInfo.builder()
                                        .name(entityName)
                                        .metadataFilter(filter)
                                        .newData(newData)
                                        .build()
                        );
                    } else {
                        graphStore.addVertex(
                                GraphVertex.builder()
                                        .label(entityType)
                                        .name(entityName)
                                        .textSegmentId(textSegmentId)
                                        .description(entityDescription)
                                        .metadata(metadata)
                                        .build()
                        );
                    }
                } else if (recordAttributes.length >= 4 && (recordAttributes[0].contains("\"relationship\"") || recordAttributes[0].contains("\"关系\""))) {
                    String sourceName = AdiStringUtil.clearStr(recordAttributes[1].toUpperCase());
                    String targetName = AdiStringUtil.clearStr(recordAttributes[2].toUpperCase());
                    String edgeDescription = AdiStringUtil.clearStr(recordAttributes[3]);
                    log.info("Relationship sourceName:{},targetName:{},edgeDescription:{}", sourceName, targetName, edgeDescription);
                    String chunkId = AdiStringUtil.clearStr(textSegmentId);

                    double weight = 1.0;
                    if (recordAttributes.length > 4) {
                        String tailRecord = recordAttributes[recordAttributes.length - 1];
                        weight = NumberUtils.toDouble(tailRecord, 1.0);
                    }

                    // 账本：边行。端点按字典序规范化——图库对边的查找/合并是无向的，
                    // 正反两次抽取必须收敛到同一元素键，否则独占判定会误删他段贡献
                    String from = sourceName.compareTo(targetName) <= 0 ? sourceName : targetName;
                    String to = sourceName.compareTo(targetName) <= 0 ? targetName : sourceName;
                    addEdgeRow(edgeRows, edgeRow(kbUuid, docUuid, textSegmentId, from, to, edgeDescription, weight));
                    // 账本：端点不变式——边的每个贡献者必然也是两端顶点的贡献者（独占顶点删除无损的前提）
                    addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, sourceName, null, null));
                    addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, targetName, null, null));

                    //Source vertex
                    GraphVertex source = graphStore.getVertex(
                            GraphVertexSearch.builder()
                                    .names(List.of(sourceName))
                                    .metadataFilter(filter)
                                    .build()
                    );
                    if (null == source) {
                        graphStore.addVertex(
                                GraphVertex.builder()
                                        .name(sourceName)
                                        .textSegmentId(chunkId)
                                        .metadata(metadata)
                                        .build()
                        );
                    }
                    //Target vertex
                    GraphVertex target = graphStore.getVertex(
                            GraphVertexSearch.builder()
                                    .names(List.of(targetName))
                                    .metadataFilter(filter)
                                    .build()
                    );
                    if (null == target) {
                        graphStore.addVertex(
                                GraphVertex.builder()
                                        .name(targetName)
                                        .textSegmentId(chunkId)
                                        .metadata(metadata)
                                        .build()
                        );
                    }
                    //Edge
                    GraphEdgeSearch search = new GraphEdgeSearch();
                    search.setSource(GraphSearchCondition.builder()
                            .names(List.of(sourceName))
                            .metadataFilter(filter)
                            .build());
                    search.setTarget(GraphSearchCondition.builder()
                            .names(List.of(targetName))
                            .metadataFilter(filter)
                            .build());
                    Triple<GraphVertex, GraphEdge, GraphVertex> graphEdgeWithVertices = graphStore.getEdge(search);
                    if (null != graphEdgeWithVertices) {
                        GraphEdge existGraphEdge = graphEdgeWithVertices.getMiddle();
                        weight += existGraphEdge.getWeight();
                        GraphEdgeEditInfo graphEdgeEditInfo = new GraphEdgeEditInfo();
                        graphEdgeEditInfo.setSourceFilter(GraphSearchCondition.builder()
                                .names(List.of(sourceName))
                                .metadataFilter(filter)
                                .build());
                        graphEdgeEditInfo.setTargetFilter(GraphSearchCondition.builder()
                                .names(List.of(targetName))
                                .metadataFilter(filter)
                                .build());
                        graphEdgeEditInfo.setEdge(GraphEdge.builder()
                                .textSegmentId(existGraphEdge.getTextSegmentId() + "," + chunkId)
                                .description(existGraphEdge.getDescription() + "\n" + edgeDescription)
                                .weight(weight)
                                .build());
                        graphStore.updateEdge(graphEdgeEditInfo);

                        appendExistsToNewOne(existGraphEdge.getMetadata(), metadata);
                    } else {
//Create if not exists
                        //检查sourceName的节点是否存在，不存在则创建
                        checkOrCreateVertex("", sourceName, chunkId, filter, metadata);
                        checkOrCreateVertex("", targetName, chunkId, filter, metadata);
                        GraphEdgeAddInfo addInfo = new GraphEdgeAddInfo();
                        addInfo.setEdge(GraphEdge.builder()
                                .sourceName(sourceName)
                                .targetName(targetName)
                                .weight(weight)
                                .metadata(metadata)
                                .textSegmentId(chunkId)
                                .description(edgeDescription)
                                .build());
                        addInfo.setSourceFilter(GraphSearchCondition.builder()
                                .names(List.of(sourceName))
                                .metadataFilter(filter)
                                .build());
                        addInfo.setTargetFilter(GraphSearchCondition.builder()
                                .names(List.of(targetName))
                                .metadataFilter(filter)
                                .build());
                        graphStore.addEdge(addInfo);
                    }
                }
            }
        }

        saveProvenanceRows(vertexRows, edgeRows);
        log.info("Finished storing {} extracted segments into the graph store", extracted.size());
    }

    /**
     * 账本落库（双写的账本侧，攒批 + ON CONFLICT 幂等）。失败仅记录日志、不阻断抽取：
     * 漏记的贡献按"账本为准"原则不参与段级操作判定，可由文档重跑图谱收敛。
     */
    private void saveProvenanceRows(Map<String, DocumentGraphVertex> vertexRows, Map<String, DocumentGraphEdge> edgeRows) {
        if (vertexRows.isEmpty() && edgeRows.isEmpty()) {
            return;
        }
        try {
            SpringUtil.getBean(DocumentGraphProvenanceService.class)
                    .saveContributions(new ArrayList<>(vertexRows.values()), new ArrayList<>(edgeRows.values()));
        } catch (Exception e) {
            log.error("Save graph provenance rows failed, vertices:{}, edges:{}", vertexRows.size(), edgeRows.size(), e);
        }
    }

    /**
     * 键内去重收藏账本顶点行：实体记录与关系端点重复时，保留带类型与描述片段的富信息行
     */
    private void addVertexRow(Map<String, DocumentGraphVertex> rows, DocumentGraphVertex row) {
        String key = row.getKbUuid() + "|" + row.getName() + "|" + row.getSegmentUuid();
        DocumentGraphVertex exist = rows.get(key);
        if (exist == null) {
            rows.put(key, row);
        } else if (exist.getDescription() == null && row.getDescription() != null) {
            exist.setEntityType(row.getEntityType());
            exist.setDescription(row.getDescription());
        }
    }

    private void addEdgeRow(Map<String, DocumentGraphEdge> rows, DocumentGraphEdge row) {
        rows.putIfAbsent(row.getKbUuid() + "|" + row.getSourceName() + "|" + row.getTargetName() + "|" + row.getSegmentUuid(), row);
    }

    private DocumentGraphVertex vertexRow(String kbUuid, String docUuid, String segmentUuid, String name, String entityType, String description) {
        DocumentGraphVertex row = new DocumentGraphVertex();
        row.setKbUuid(kbUuid);
        row.setDocUuid(docUuid);
        row.setSegmentUuid(segmentUuid);
        row.setName(name);
        row.setEntityType(entityType);
        row.setDescription(description);
        return row;
    }

    private DocumentGraphEdge edgeRow(String kbUuid, String docUuid, String segmentUuid, String sourceName, String targetName, String description, double weight) {
        DocumentGraphEdge row = new DocumentGraphEdge();
        row.setKbUuid(kbUuid);
        row.setDocUuid(docUuid);
        row.setSegmentUuid(segmentUuid);
        row.setSourceName(sourceName);
        row.setTargetName(targetName);
        row.setDescription(description);
        row.setWeight(weight);
        return row;
    }

    /**
     * metadata记录的值为Map，如：kb_uuid=>123,kb_item_uuid=>22222,3333，其中类似 3333 的值是追加的，超过最大限度时丢弃最早的数据
     * TODO 重构以记录所有追加的值
     *
     * @param existMetadata 已存在的metadata
     * @param newMetadata   新的metadata
     */
    private void appendExistsToNewOne(Map<String, Object> existMetadata, Map<String, Object> newMetadata) {
        for (String columnName : appendColumns) {
            String existValue = String.valueOf(existMetadata.get(columnName));
            String newValue = String.valueOf(newMetadata.get(columnName));
            if (StringUtils.isNotBlank(existValue) && !existValue.contains(newValue)) {
                String cleanedTxt = existValue.replaceAll("[\\s\"/\\\\]", "");
                newMetadata.put(columnName, checkAndRemoveOldest(cleanedTxt) + "," + newValue);
            }
        }
    }

    private String checkAndRemoveOldest(String cleanedTxt) {
        if (StringUtils.isBlank(cleanedTxt)) {
            return cleanedTxt;
        }
        String result = cleanedTxt;
        while (result.length() > MAX_METADATA_VALUE_LENGTH) {
            String[] existValues = result.split(",", 2); // Only split into 2 parts
            if (existValues.length <= 1) {
                return result.substring(0, MAX_METADATA_VALUE_LENGTH);
            }
            result = existValues[1]; // Take everything after the first comma
        }
        return result;
    }

    private void checkOrCreateVertex(String label, String name, String textSegmentId, Filter metadataFilter, Map<String, Object> metadata) {
        List<GraphVertex> existVertices = graphStore.searchVertices(
                GraphVertexSearch.builder()
                        .label(label)
                        .limit(1)
                        .names(List.of(name))
                        .metadataFilter(metadataFilter)
                        .build()
        );
        if (CollectionUtils.isEmpty(existVertices)) {
            graphStore.addVertex(
                    GraphVertex.builder()
                            .label(label)
                            .name(name)
                            .textSegmentId(textSegmentId)
                            .metadata(metadata)
                            .build()
            );
        }
    }
}
