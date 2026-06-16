package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.service.CharacterMessageRefEmbeddingService;
import com.moyz.adi.common.service.CharacterMessageRefGraphService;
import com.moyz.adi.common.service.CharacterMessageRefMemoryEmbeddingService;
import com.moyz.adi.common.service.CharacterMessageService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Message CRUD controller: handles message query, delete, and reference viewing only.
 */
@Tag(name = "Character Message")
@RestController
@RequestMapping("/character/message")
@Validated
public class CharacterMessageController {

    @Resource
    private CharacterMessageService characterMessageService;

    @Resource
    private CharacterMessageRefEmbeddingService characterMessageRefEmbeddingService;

    @Resource
    private CharacterMessageRefMemoryEmbeddingService characterMessageRefMemoryEmbeddingService;

    @Resource
    private CharacterMessageRefGraphService characterMessageRefGraphService;

    @Operation(summary = "根据音频uuid获取对应的文本 | Get Text by Audio UUID")
    @GetMapping("/text/{audioUuid}")
    public String getTextByAudioUuid(@PathVariable String audioUuid) {
        return characterMessageService.getTextByAudioUuid(audioUuid);
    }

    @GetMapping("/knowledge-embedding-ref/{uuid}")
    public List<RefEmbeddingDto> embeddingRef(@PathVariable String uuid) {
        return characterMessageRefEmbeddingService.listRefEmbeddings(uuid);
    }

    @GetMapping("/memory-embedding-ref/{msgUuid}")
    public List<RefEmbeddingDto> memoryEmbeddingRef(@PathVariable String msgUuid) {
        return characterMessageRefMemoryEmbeddingService.listRefEmbeddings(msgUuid);
    }

    @GetMapping("/graph-ref/{uuid}")
    public RefGraphDto graphRef(@PathVariable String uuid) {
        return characterMessageRefGraphService.getByMsgUuid(uuid);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDelete(@PathVariable String uuid) {
        return characterMessageService.softDelete(uuid);
    }

}
