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
     * <p>
     * 三阶段执行，顺序是漂移裁决的关键（账本为准）：
     * 1. 纯解析（无图库 IO）：LLM 响应 -> 中间结构 + 账本贡献行；
     * 2. 账本先行落库：账本是事实源，写不进去就不写派生索引--失败抛异常由上游标记 FAIL。
     *    若反序（图库先写、账本失败），漂移方向是"账本少记"，清理时会把他段贡献误判为独占而删除；
     *    账本先行的漂移方向是"账本多记"，退化为对图库不存在元素的 no-op 删除，安全；
     * 3. 图库合并写入（检索缓存）。
     */
    public void ingestExtracted(List<Triple<TextSegment, String, String>> extracted) {
        log.info("Starting to store {} extracted segments into the graph store", extracted.size());
        Map<String, DocumentGraphVertex> vertexRows = new LinkedHashMap<>();
        Map<String, DocumentGraphEdge> edgeRows = new LinkedHashMap<>();
        List<ParsedSegment> parsedSegments = new ArrayList<>();
        for (Triple<TextSegment, String, String> triple : extracted) {
            ParsedSegment parsed = parseSegment(triple, vertexRows, edgeRows);
            if (parsed != null) {
                parsedSegments.add(parsed);
            }
        }
        saveProvenanceRows(vertexRows, edgeRows);
        for (ParsedSegment parsed : parsedSegments) {
            writeToGraphStore(parsed);
        }
        log.info("Finished storing {} extracted segments into the graph store", extracted.size());
    }

    /**
     * 解析单段抽取响应为中间结构，同时收集账本贡献行。response 为空返回 null。
     */
    private ParsedSegment parseSegment(Triple<TextSegment, String, String> triple,
                                       Map<String, DocumentGraphVertex> vertexRows,
                                       Map<String, DocumentGraphEdge> edgeRows) {
        TextSegment segment = triple.getLeft();
        String textSegmentId = triple.getMiddle();
        String response = triple.getRight();
        Map<String, Object> metadata = segment.metadata().toMap();
        String kbUuid = String.valueOf(metadata.get(AdiConstant.MetadataKey.KB_UUID));
        String docUuid = String.valueOf(metadata.get(AdiConstant.MetadataKey.KB_ITEM_UUID));
        log.info("Graph response:{}", response);
        if (StringUtils.isBlank(response)) {
            log.warn("Response is empty, segmentId:{}", textSegmentId);
            return null;
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

        List<ParsedEntity> entities = new ArrayList<>();
        List<ParsedRelationship> relationships = new ArrayList<>();
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
                entities.add(new ParsedEntity(entityName, entityType, entityDescription));
                addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, entityName, entityType, entityDescription));
            } else if (recordAttributes.length >= 4 && (recordAttributes[0].contains("\"relationship\"") || recordAttributes[0].contains("\"关系\""))) {
                String sourceName = AdiStringUtil.clearStr(recordAttributes[1].toUpperCase());
                String targetName = AdiStringUtil.clearStr(recordAttributes[2].toUpperCase());
                String edgeDescription = AdiStringUtil.clearStr(recordAttributes[3]);
                log.info("Relationship sourceName:{},targetName:{},edgeDescription:{}", sourceName, targetName, edgeDescription);
                double weight = 1.0;
                if (recordAttributes.length > 4) {
                    String tailRecord = recordAttributes[recordAttributes.length - 1];
                    weight = NumberUtils.toDouble(tailRecord, 1.0);
                }
                relationships.add(new ParsedRelationship(sourceName, targetName, edgeDescription, weight));
                // 账本：边行。端点按字典序规范化--图库对边的查找/合并是无向的，
                // 正反两次抽取必须收敛到同一元素键，否则独占判定会误删他段贡献
                String from = sourceName.compareTo(targetName) <= 0 ? sourceName : targetName;
                String to = sourceName.compareTo(targetName) <= 0 ? targetName : sourceName;
                addEdgeRow(edgeRows, edgeRow(kbUuid, docUuid, textSegmentId, from, to, edgeDescription, weight));
                // 账本：端点不变式--边的每个贡献者必然也是两端顶点的贡献者（独占顶点删除无损的前提）
                addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, sourceName, null, null));
                addVertexRow(vertexRows, vertexRow(kbUuid, docUuid, textSegmentId, targetName, null, null));
            }
        }
        return new ParsedSegment(metadata, filter, textSegmentId, entities, relationships);
    }

    /**
     * 阶段三：把解析结果合并写入图库（检索缓存）。逻辑与重构前一致：实体不存在则建、
     * 存在则追加 textSegmentId/description/metadata；关系端点缺失则补建，边存在则累加、不存在则新建。
     */
    private void writeToGraphStore(ParsedSegment parsed) {
        Map<String, Object> metadata = parsed.metadata();
        Filter filter = parsed.filter();
        String textSegmentId = parsed.textSegmentId();
        String chunkId = AdiStringUtil.clearStr(textSegmentId);
        for (ParsedEntity entity : parsed.entities()) {
            //实体如果不存在图数据库中，插入一个新的实体，否则追加textSegmentId、description以及metadata中指定的内容
            List<GraphVertex> existVertices = graphStore.searchVertices(
                    GraphVertexSearch.builder()
                            .label(entity.type())
                            .limit(1)
                            .names(List.of(entity.name()))
                            .metadataFilter(filter)
                            .build()
            );
            if (CollectionUtils.isNotEmpty(existVertices)) {
                GraphVertex existVertex = existVertices.get(0);
                String newTextSegmentId = existVertex.getTextSegmentId() + "," + textSegmentId;
                String newDesc = existVertex.getDescription() + "\n" + entity.description();

                appendExistsToNewOne(existVertex.getMetadata(), metadata);
                GraphVertex newData = GraphVertex.builder().textSegmentId(newTextSegmentId).description(newDesc).metadata(metadata).build();
                graphStore.updateVertex(
                        GraphVertexUpdateInfo.builder()
                                .name(entity.name())
                                .metadataFilter(filter)
                                .newData(newData)
                                .build()
                );
            } else {
                graphStore.addVertex(
                        GraphVertex.builder()
                                .label(entity.type())
                                .name(entity.name())
                                .textSegmentId(textSegmentId)
                                .description(entity.description())
                                .metadata(metadata)
                                .build()
                );
            }
        }
        for (ParsedRelationship relationship : parsed.relationships()) {
            String sourceName = relationship.sourceName();
            String targetName = relationship.targetName();
            String edgeDescription = relationship.description();
            double weight = relationship.weight();

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

    /**
     * 账本落库（双写的账本侧，攒批 + ON CONFLICT 幂等）。账本是事实源：写失败抛异常、
     * 阻断后续图库写入（上游 indexingGraph/asyncReGraphSegment 会捕获并标记 FAIL）--
     * 图库成功而账本漏记的漂移方向是"误删他段贡献"，必须避免。
     */
    private void saveProvenanceRows(Map<String, DocumentGraphVertex> vertexRows, Map<String, DocumentGraphEdge> edgeRows) {
        if (vertexRows.isEmpty() && edgeRows.isEmpty()) {
            return;
        }
        SpringUtil.getBean(DocumentGraphProvenanceService.class)
                .saveContributions(new ArrayList<>(vertexRows.values()), new ArrayList<>(edgeRows.values()));
    }

    /**
     * 解析阶段中间结构：段抽取响应拆解为实体/关系记录（图库写入阶段只读，不重复解析）
     */
    private record ParsedEntity(String name, String type, String description) {
    }

    private record ParsedRelationship(String sourceName, String targetName, String description, double weight) {
    }

    private record ParsedSegment(Map<String, Object> metadata, Filter filter, String textSegmentId,
                                  List<ParsedEntity> entities, List<ParsedRelationship> relationships) {
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
