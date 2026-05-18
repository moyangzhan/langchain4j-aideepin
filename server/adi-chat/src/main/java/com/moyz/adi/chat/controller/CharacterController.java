package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.CharacterAddReq;
import com.moyz.adi.common.dto.CharacterDto;
import com.moyz.adi.common.dto.CharacterEditReq;
import com.moyz.adi.common.dto.CharacterMsgListResp;
import com.moyz.adi.common.service.CharacterService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import org.hibernate.validator.constraints.Length;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
* Character controller
 * 角色controller
 */
@Tag(name = "角色controller | Character Controller", description = "角色controller | Character Controller")
@RequestMapping("/character")
@RestController
@Validated
public class CharacterController {

    @Resource
    private CharacterService characterService;

    @Operation(summary = "获取当前用户所有的角色 | List All Characters")
    @GetMapping("/list")
    public List<CharacterDto> list() {
        return characterService.listByUser();
    }

    @Operation(summary = "查询某个角色的信息列表 | List Character Messages")
    @GetMapping("/{uuid}")
    public CharacterMsgListResp detail(
            @Parameter(name = "对话uuid | Character UUID") @PathVariable @NotBlank(message = "Character UUID cannot be empty") String uuid
            , @Parameter(name = "最大uuid | Max Message UUID") @RequestParam String maxMsgUuid
            , @Parameter(name = "每页数量 | Page Size") @RequestParam @Min(1) @Max(100) int pageSize) {
        return characterService.detail(uuid, maxMsgUuid, pageSize);
    }

    @PostMapping("/add")
    public CharacterDto add(@RequestBody @Validated CharacterAddReq characterAddReq) {
        return characterService.add(characterAddReq);
    }

    @Operation(summary = "根据预设角色创建用户自己的角色 | Create Character from Preset")
    @PostMapping("/addByPreset")
    public CharacterDto addByPreset(@Length(min = 32, max = 32) @RequestParam String presetUuid) {
        return characterService.addByPresetCharacter(presetUuid);
    }

    @PostMapping("/edit/{uuid}")
    public boolean edit(@PathVariable String uuid, @RequestBody CharacterEditReq characterEditReq) {
        return characterService.edit(uuid, characterEditReq);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDel(@PathVariable String uuid) {
        return characterService.softDel(uuid);
    }
}
