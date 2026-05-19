package com.moyz.adi.chat.controller.externalapi.v1;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbQaDto;
import com.moyz.adi.common.dto.QARecordReq;
import com.moyz.adi.common.dto.extapi.ExtApiKbQaReq;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.service.KnowledgeBaseQaService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "External API - Knowledge Base")
@RestController
@RequestMapping("/api/v1/knowledge")
@Validated
public class ExtKnowledgeBaseController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseQaService knowledgeBaseQaService;

    @Operation(summary = "Knowledge base Q&A")
    @PostMapping
    public Object knowledgeBaseQa(@RequestBody @Validated ExtApiKbQaReq req) {
        User user = ThreadContext.getCurrentUser();
        String kbUuid = ThreadContext.getExtApiEntityUuid();

        KnowledgeBase knowledgeBase = knowledgeBaseService.getOrThrow(kbUuid);

        QARecordReq qaRecordReq = new QARecordReq();
        qaRecordReq.setQuestion(req.getQuery());
        KbQaDto qaDto = knowledgeBaseQaService.add(knowledgeBase, qaRecordReq);

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return knowledgeBaseService.blockingAsk(user, knowledgeBase, qaDto);
        }

        return knowledgeBaseService.sseAsk(qaDto.getUuid());
    }
}
