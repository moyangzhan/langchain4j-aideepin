package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.DrawDto;
import com.moyz.adi.common.dto.DrawListResp;
import com.moyz.adi.common.service.DrawService;
import com.moyz.adi.common.service.DrawStarService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.NotBlank;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/draw/star")
@Validated
public class DrawStarController {

    @Resource
    private DrawService drawService;

    @Resource
    private DrawStarService drawStarService;

    @GetMapping("/mine")
    public DrawListResp myStars(@RequestParam Long maxId, @RequestParam int pageSize) {
        return drawService.listFav(maxId, pageSize);
    }

    @Operation(summary = "将绘图任务设置为公开或私有")
    @PostMapping("/toggle/{uuid}")
    public DrawDto star(@PathVariable @NotBlank String uuid) {
        DrawDto draw = drawService.getOrThrow(uuid);
        drawStarService.toggle(draw.getId());
        return draw;
    }
}
