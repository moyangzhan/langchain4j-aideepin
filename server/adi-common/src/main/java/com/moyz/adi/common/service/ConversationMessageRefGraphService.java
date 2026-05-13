package com.moyz.adi.common.service;

import com.alibaba.dashscope.utils.JsonUtils;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.ConversationMessageRefGraph;
import com.moyz.adi.common.mapper.ConversationMessageRefGraphMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class ConversationMessageRefGraphService extends ServiceImpl<ConversationMessageRefGraphMapper, ConversationMessageRefGraph> {

    public RefGraphDto getByMsgUuid(String messageUuid) {
        List<ConversationMessageRefGraph> list = this.getBaseMapper().listByMsgUuid(messageUuid);
        if (list.isEmpty()) {
            return RefGraphDto
                    .builder()
                    .vertices(Collections.emptyList())
                    .edges(Collections.emptyList())
                    .entitiesFromQuestion(Collections.emptyList())
                    .build();
        }
        ConversationMessageRefGraph refGraph = list.get(0);

        RefGraphDto result = new RefGraphDto();
        String graphStr = refGraph.getGraphFromStore();
        if (StringUtils.isNotBlank(graphStr)) {
            result = JsonUtils.fromJson(graphStr, RefGraphDto.class);
        }
        result.setEntitiesFromQuestion(Arrays.stream(refGraph.getGraphFromLlm().split(",")).filter(StringUtils::isNotBlank).toList());
        if (null == result.getVertices()) {
            result.setVertices(Collections.emptyList());
        }
        if (null == result.getEdges()) {
            result.setEdges(Collections.emptyList());
        }
        return result;
    }

}
