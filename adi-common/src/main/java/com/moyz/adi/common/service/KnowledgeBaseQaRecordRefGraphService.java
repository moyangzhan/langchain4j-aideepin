package com.moyz.adi.common.service;

import com.alibaba.dashscope.utils.JsonUtils;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.KbQaRecordRefGraphDto;
import com.moyz.adi.common.entity.KnowledgeBaseQaRecordRefGraph;
import com.moyz.adi.common.mapper.KnowledgeBaseQaRecordRefGraphMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class KnowledgeBaseQaRecordRefGraphService extends ServiceImpl<KnowledgeBaseQaRecordRefGraphMapper, KnowledgeBaseQaRecordRefGraph> {

    public KbQaRecordRefGraphDto getByQaUuid(String aqRecordUuid) {
        List<KnowledgeBaseQaRecordRefGraph> list = this.getBaseMapper().listByQaUuid(aqRecordUuid);
        if (list.isEmpty()) {
            return KbQaRecordRefGraphDto
                    .builder()
                    .vertices(Collections.emptyList())
                    .edges(Collections.emptyList())
                    .entitiesFromLlm(Collections.emptyList())
                    .build();
        }
        KnowledgeBaseQaRecordRefGraph refGraph = list.get(0);

        KbQaRecordRefGraphDto result = new KbQaRecordRefGraphDto();
        String graphStr = refGraph.getGraphFromStore();
        if (StringUtils.isNotBlank(graphStr)) {
            result = JsonUtils.fromJson(graphStr, KbQaRecordRefGraphDto.class);
        }
        result.setEntitiesFromLlm(Arrays.asList(refGraph.getGraphFromLlm().split(",")).stream().filter(StringUtils::isNotBlank).toList());
        if (null == result.getVertices()) {
            result.setVertices(Collections.emptyList());
        }
        if (null == result.getEdges()) {
            result.setEdges(Collections.emptyList());
        }
        return result;
    }
}
