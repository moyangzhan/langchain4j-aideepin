package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.FileCleanupDto;
import com.moyz.adi.common.dto.FileSearchReq;
import com.moyz.adi.common.service.FileGcService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * File cleanup
 */
@Tag(name = "文件清理 | File Cleanup", description = "文件清理 | File Cleanup")
@RestController
@RequestMapping("/admin/file")
@Validated
public class AdminFileController {

    @Resource
    private FileGcService fileGcService;

    @Operation(summary = "搜索无引用文件 | Search Unreferenced Files")
    @PostMapping("/search")
    public Page<FileCleanupDto> search(@RequestBody FileSearchReq req,
                                       @NotNull @Min(1) Integer currentPage,
                                       @NotNull @Min(10) Integer pageSize) {
        return fileGcService.search(currentPage, pageSize, req.getName(), req.getUserName());
    }

    @Operation(summary = "删除无引用文件 | Delete Unreferenced File")
    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        fileGcService.deleteForCleanup(uuid);
        return true;
    }
}
