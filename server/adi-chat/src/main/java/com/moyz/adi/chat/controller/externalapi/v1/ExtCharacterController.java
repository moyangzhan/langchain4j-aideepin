package com.moyz.adi.chat.controller.externalapi.v1;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.AskReq;
import com.moyz.adi.common.dto.extapi.ExtApiChatReq;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.AiModelService;
import com.moyz.adi.common.service.CharacterMessageService;
import com.moyz.adi.common.service.CharacterService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.A_MODEL_NOT_AVAILABLE;

@Tag(name = "External API - Character")
@RestController
@RequestMapping("/ext/v1/character")
@Validated
public class ExtCharacterController {

    @Resource
    private CharacterMessageService characterMessageService;

    @Resource
    private CharacterService characterService;

    @Resource
    private AiModelService aiModelService;

    @Operation(summary = "Chat with a character")
    @PostMapping
    public Object chatMessages(@RequestBody @Validated ExtApiChatReq req) {
        String characterUuid = ThreadContext.getExtApiEntityUuid();

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

        if (StringUtils.isNotBlank(req.getModel())) {
            AiModel aiModel = aiModelService.getByName(req.getModel());
            if (null == aiModel || !aiModel.getIsEnable()) {
                throw new BaseException(A_MODEL_NOT_AVAILABLE);
            }
            askReq.setModelName(req.getModel());
        }

        if ("blocking".equalsIgnoreCase(req.getResponseMode())) {
            return characterMessageService.blockingAsk(askReq);
        }

        return characterMessageService.sseAsk(askReq);
    }
}
