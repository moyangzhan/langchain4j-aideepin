package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.ConvAddReq;
import com.moyz.adi.common.dto.ConvDto;
import com.moyz.adi.common.dto.ConvEditReq;
import com.moyz.adi.common.dto.ConvMsgListResp;
import com.moyz.adi.common.service.ConversationService;
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
 * 对话controller
 */
@Tag(name = "对话controller", description = "对话controller")
@RequestMapping("/conversation")
@RestController
@Validated
public class ConversationController {

    @Resource
    private ConversationService conversationService;

    @Operation(summary = "获取当前用户所有的对话")
    @GetMapping("/list")
    public List<ConvDto> list() {
        return conversationService.listByUser();
    }

    @Operation(summary = "查询某个对话的信息列表")
    @GetMapping("/{uuid}")
    public ConvMsgListResp detail(
            @Parameter(name = "对话uuid") @PathVariable @NotBlank(message = "对话uuid不能为空") String uuid
            , @Parameter(name = "最大uuid") @RequestParam String maxMsgUuid
            , @Parameter(name = "每页数量") @RequestParam @Min(1) @Max(100) int pageSize) {
        return conversationService.detail(uuid, maxMsgUuid, pageSize);
    }

    @PostMapping("/add")
    public ConvDto add(@RequestBody @Validated ConvAddReq convAddReq) {
        return conversationService.add(convAddReq);
    }

    @Operation(summary = "根据预设会话创建用户自己的会话")
    @PostMapping("/addByPreset")
    public ConvDto addByPreset(@Length(min = 32, max = 32) @RequestParam String presetUuid) {
        return conversationService.addByPresetConv(presetUuid);
    }

    @PostMapping("/edit/{uuid}")
    public boolean edit(@PathVariable String uuid, @RequestBody ConvEditReq convEditReq) {
        return conversationService.edit(uuid, convEditReq);
    }

    @PostMapping("/del/{uuid}")
    public boolean softDel(@PathVariable String uuid) {
        return conversationService.softDel(uuid);
    }
}
