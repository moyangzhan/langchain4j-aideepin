package com.moyz.adi.chat.controller.externalapi.v1;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.openapi.OpenApiChatReq;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.CharacterMessageService;
import com.moyz.adi.common.service.CharacterService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;

@Tag(name = "External API - Character")
@RestController
@RequestMapping("/api/v1/character")
@Validated
public class ExtCharacterController {

    @Resource
    private CharacterMessageService characterMessageService;

    @Resource
    private CharacterService characterService;

    @Operation(summary = "Chat with a character")
    @PostMapping
    public Object chatMessages(@RequestBody @Validated OpenApiChatReq req) {
        String characterUuid = ThreadContext.getOpenApiEntityUuid();

        Character character = characterService.lambdaQuery()
                .eq(Character::getUuid, characterUuid)
                .eq(Character::getIsDeleted, false)
                .one();
        if (null == character) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }

        AskReq askReq = new AskReq();
        askReq.setCharacterUuid(characterUuid);
        askReq.setPrompt(req.getQuery());

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return characterMessageService.blockingAsk(askReq);
        }

        return characterMessageService.sseAsk(askReq);
    }
}
