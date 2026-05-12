package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.ConvDto;
import com.moyz.adi.common.dto.ConvEditReq;
import com.moyz.adi.common.dto.ConvSearchReq;
import com.moyz.adi.common.dto.UserEditReq;
import com.moyz.adi.common.service.ConversationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
* Conversation management
 * 聊天会话管理
 */
@Tag(name = "会话管理 | Conversation Management", description = "会话管理 | Conversation Management")
@RestController
@RequestMapping("/admin/conv")
@Validated
public class AdminConvController {

    @Resource
    private ConversationService conversationService;

    @Operation(summary = "搜索会话 | Search Conversations")
    @PostMapping("/search")
    public Page<ConvDto> search(@RequestBody ConvSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return conversationService.search(searchReq, currentPage, pageSize);
    }

    @Operation(summary = "编辑会话 | Edit Conversation")
    @PostMapping("/edit/{uuid}")
    public void edit(@PathVariable String uuid, @Validated @RequestBody ConvEditReq editReq) {
        conversationService.edit(uuid, editReq);
    }

    @Operation(summary = "删除会话 | Delete Conversation")
    @PostMapping("/del/{uuid}")
    public boolean softDel(@PathVariable String uuid) {
        return conversationService.softDel(uuid);
    }
}
