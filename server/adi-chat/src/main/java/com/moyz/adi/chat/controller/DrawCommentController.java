package com.moyz.adi.chat.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.DrawCommentDto;
import com.moyz.adi.common.service.DrawCommentService;
import com.moyz.adi.common.service.DrawService;
import com.moyz.adi.common.vo.DrawCommentAddReq;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/draw/comment")
@Validated
public class DrawCommentController {

    @Resource
    private DrawService drawService;

    @Resource
    private DrawCommentService drawCommentService;

    @GetMapping("/list/{uuid}")
    public Page<DrawCommentDto> listByPage(@PathVariable String uuid, @RequestParam(defaultValue = "1") Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return drawService.listCommentsByPage(uuid, currentPage, pageSize);
    }

    @PostMapping("/add")
    public DrawCommentDto save(@RequestBody DrawCommentAddReq commentAddReq) {
        return drawService.addComment(commentAddReq.getDrawUuid(), commentAddReq.getComment());
    }

    @PostMapping("/del")
    public boolean del(@RequestParam Long id) {
        return drawCommentService.softDel(id);
    }
}
