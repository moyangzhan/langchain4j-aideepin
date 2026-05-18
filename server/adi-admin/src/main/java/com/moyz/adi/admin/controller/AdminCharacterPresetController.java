package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.CharacterPresetAddReq;
import com.moyz.adi.common.dto.CharacterPresetEditReq;
import com.moyz.adi.common.dto.CharacterPresetSearchReq;
import com.moyz.adi.common.entity.CharacterPreset;
import com.moyz.adi.common.service.CharacterPresetService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "对话预设管理 | Character Preset Management", description = "对话预设管理 | Character Preset Management")
@RestController
@RequestMapping("/admin/character-preset")
@Validated
public class AdminCharacterPresetController {

    @Resource
    private CharacterPresetService characterPresetService;

    @Operation(summary = "搜索对话预设 | Search Character Presets")
    @PostMapping("/search")
    public Page<CharacterPreset> page(@RequestBody CharacterPresetSearchReq keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return characterPresetService.search(keyword.getTitle(), currentPage, pageSize);
    }

    @Operation(summary = "添加对话预设 | Add Character Preset")
    @PostMapping("/addOne")
    public CharacterPreset addOne(@RequestBody CharacterPresetAddReq presetAddReq) {
        return characterPresetService.addOne(presetAddReq);
    }

    @Operation(summary = "编辑对话预设 | Edit Character Preset")
    @PostMapping("/edit/{uuid}")
    public boolean edit(@PathVariable String uuid, @RequestBody CharacterPresetEditReq editReq) {
        return characterPresetService.edit(uuid, editReq);
    }

    @Operation(summary = "删除对话预设 | Delete Character Preset")
    @PostMapping("/del/{uuid}")
    public void delete(@PathVariable String uuid) {
        characterPresetService.softDel(uuid);
    }
}
