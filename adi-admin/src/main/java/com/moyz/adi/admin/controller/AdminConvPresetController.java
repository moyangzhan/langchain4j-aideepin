package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.ConvPresetAddReq;
import com.moyz.adi.common.dto.ConvPresetEditReq;
import com.moyz.adi.common.dto.ConvPresetSearchReq;
import com.moyz.adi.common.entity.ConversationPreset;
import com.moyz.adi.common.service.ConversationPresetService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Tag(name = "对话预设管理 | Conversation Preset Management", description = "对话预设管理 | Conversation Preset Management")
@RestController
@RequestMapping("/admin/conv-preset")
@Validated
public class AdminConvPresetController {

    @Resource
    private ConversationPresetService conversationPresetService;

    @Operation(summary = "搜索对话预设 | Search Conversation Presets")
    @PostMapping("/search")
    public Page<ConversationPreset> page(@RequestBody ConvPresetSearchReq keyword, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return conversationPresetService.search(keyword.getTitle(), currentPage, pageSize);
    }

    @Operation(summary = "添加对话预设 | Add Conversation Preset")
    @PostMapping("/addOne")
    public ConversationPreset addOne(@RequestBody ConvPresetAddReq presetAddReq) {
        return conversationPresetService.addOne(presetAddReq);
    }

    @Operation(summary = "编辑对话预设 | Edit Conversation Preset")
    @PostMapping("/edit/{uuid}")
    public boolean edit(@PathVariable String uuid, @RequestBody ConvPresetEditReq editReq) {
        return conversationPresetService.edit(uuid, editReq);
    }

    @Operation(summary = "删除对话预设 | Delete Conversation Preset")
    @PostMapping("/del/{uuid}")
    public void delete(@PathVariable String uuid) {
        conversationPresetService.softDel(uuid);
    }
}
