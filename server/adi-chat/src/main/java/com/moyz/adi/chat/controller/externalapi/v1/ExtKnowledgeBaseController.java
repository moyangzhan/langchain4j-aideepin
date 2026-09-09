package com.moyz.adi.chat.controller.externalapi.v1;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbAskDto;
import com.moyz.adi.common.dto.KbAskReq;
import com.moyz.adi.common.dto.extapi.ExtApiKbAskReq;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.AiModelService;
import com.moyz.adi.common.service.KnowledgeBaseAskService;
import com.moyz.adi.common.service.KnowledgeBaseService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_MODEL_NOT_AVAILABLE;

@Tag(name = "External API - Knowledge Base")
@RestController
@RequestMapping("/ext/v1/knowledge")
@Validated
public class ExtKnowledgeBaseController {

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseAskService knowledgeBaseAskService;

    @Resource
    private AiModelService aiModelService;

    @Operation(summary = "Knowledge base Q&A")
    @PostMapping
    public Object knowledgeBaseQa(@RequestBody @Validated ExtApiKbAskReq req) {
        User user = ThreadContext.getCurrentUser();
        String kbUuid = ThreadContext.getExtApiEntityUuid();

        KnowledgeBase knowledgeBase = knowledgeBaseService.getOrThrow(kbUuid);

        KbAskReq askRecordReq = new KbAskReq();
        askRecordReq.setQuestion(req.getQuery());

        if (StringUtils.isNotBlank(req.getModel())) {
            AiModel aiModel = aiModelService.getByName(req.getModel());
            if (null == aiModel || !aiModel.getIsEnable()) {
                throw new BaseException(A_MODEL_NOT_AVAILABLE);
            }
            askRecordReq.setModelName(req.getModel());
        }

        KbAskDto askDto = knowledgeBaseAskService.add(knowledgeBase, askRecordReq);

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return knowledgeBaseService.blockingAsk(user, knowledgeBase, askDto);
        }

        return knowledgeBaseService.sseAsk(askDto.getUuid());
    }
}
