package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.AiSearchResp;
import com.moyz.adi.common.service.AiSearchRecordService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.web.bind.annotation.*;

@Tag(name = "AI搜索记录controller | AI Search Record Controller")
@RequestMapping("/ai-search-record/")
@RestController
public class SearchRecordController {

    @Resource
    private AiSearchRecordService aiSearchRecordService;

    @Operation(summary = "按最大ID查询 | List by Max ID")
    @GetMapping(value = "/list")
    public AiSearchResp list(@RequestParam(defaultValue = "0") Long maxId, String keyword) {
        return aiSearchRecordService.listByMaxId(maxId, keyword);
    }

    @PostMapping("/del/{uuid}")
    public boolean recordDel(@PathVariable String uuid) {
        return aiSearchRecordService.softDelete(uuid);
    }
}
