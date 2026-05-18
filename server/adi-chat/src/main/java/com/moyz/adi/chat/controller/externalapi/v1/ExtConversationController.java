package com.moyz.adi.chat.controller.externalapi.v1;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.openapi.OpenApiChatReq;
import com.moyz.adi.common.entity.Conversation;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.ConversationMessageService;
import com.moyz.adi.common.service.ConversationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;

@Tag(name = "External API - Conversation")
@RestController
@RequestMapping("/api/v1/character")
@Validated
public class ExtConversationController {

    @Resource
    private ConversationMessageService conversationMessageService;

    @Resource
    private ConversationService conversationService;

    @Operation(summary = "Chat with a conversation")
    @PostMapping
    public Object chatMessages(@RequestBody @Validated OpenApiChatReq req) {
        String convUuid = ThreadContext.getOpenApiEntityUuid();

        Conversation conversation = conversationService.lambdaQuery()
                .eq(Conversation::getUuid, convUuid)
                .eq(Conversation::getIsDeleted, false)
                .one();
        if (null == conversation) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }

        AskReq askReq = new AskReq();
        askReq.setConversationUuid(convUuid);
        askReq.setPrompt(req.getQuery());

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return conversationMessageService.blockingAsk(askReq);
        }

        return conversationMessageService.sseAsk(askReq);
    }
}
