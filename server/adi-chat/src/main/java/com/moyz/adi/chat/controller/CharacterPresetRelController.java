package com.moyz.adi.chat.controller;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.CharacterPresetRelDto;
import com.moyz.adi.common.service.CharacterPresetRelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Tag(name = "预设对话与用户对话关联关系controller | Character Preset Relation Controller")
@RequestMapping("/character-preset-rel")
@RestController
public class CharacterPresetRelController {

    @Resource
    private CharacterPresetRelService characterPresetRelService;

    @Operation(summary = "获取当前用户使用到的预设会话 | List My Used Preset Characters")
    @GetMapping("/mine")
    public List<CharacterPresetRelDto> mine(@Parameter(description = "限制数量 | Limit Count") @RequestParam(defaultValue = "100") Integer limit) {
        return characterPresetRelService.listByUser(ThreadContext.getCurrentUserId(), limit);
    }
}
