package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.ConvDto;
import com.moyz.adi.common.dto.ConvEditReq;
import com.moyz.adi.common.dto.ConvSearchReq;
import com.moyz.adi.common.dto.UserEditReq;
import com.moyz.adi.common.service.ConversationService;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 聊天会话管理
 */
@RestController
@RequestMapping("/admin/conv")
@Validated
public class AdminConvController {

    @Resource
    private ConversationService conversationService;

    @PostMapping("/search")
    public Page<ConvDto> search(@RequestBody ConvSearchReq searchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return conversationService.search(searchReq, currentPage, pageSize);
    }

    @PostMapping("/edit/{uuid}")
    public void edit(@PathVariable String uuid, @Validated @RequestBody ConvEditReq editReq) {
        conversationService.edit(uuid, editReq);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDel(@PathVariable String uuid) {
        return conversationService.softDel(uuid);
    }
}
