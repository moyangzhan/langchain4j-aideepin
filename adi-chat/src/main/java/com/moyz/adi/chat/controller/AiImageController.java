package com.moyz.adi.chat.controller;

import com.google.common.collect.Maps;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.service.AiImageService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.hibernate.validator.constraints.Length;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/ai-image")
@Validated
public class AiImageController {

    @Resource
    private AiImageService imageService;

    @PostMapping("/generation")
    public Map<String, String> generation(@RequestBody @Validated GenerateImageReq generateImageReq) {
        String uuid = imageService.createByPrompt(generateImageReq);
        return Map.of("uuid", uuid);
    }

    @PostMapping("/regenerate/{uuid}")
    public void regenerate(@PathVariable @Length(min = 32, max = 32) String uuid) {
        imageService.regenerate(uuid);
    }

    @Operation(summary = "Edit image")
    @PostMapping("/edit")
    public Map<String, String> edit(@RequestBody EditImageReq editImageReq) {
        String uuid = imageService.editByOriginalImage(editImageReq);
        return Map.of("uuid", uuid);
    }

    @Operation(summary = "Image variation")
    @PostMapping("/variation")
    public Map<String, String> variation(@RequestBody VariationImageReq variationImageReq) {
        String uuid = imageService.variationImage(variationImageReq);
        return Map.of("uuid", uuid);
    }

    @GetMapping("/list")
    public AiImagesListResp list(@RequestParam Long maxId, @RequestParam int pageSize) {
        return imageService.listAll(maxId, pageSize);
    }

    @GetMapping("/detail/{uuid}")
    public AiImageDto getOne(@PathVariable String uuid) {
        return imageService.getOne(uuid);
    }

    @GetMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        return imageService.del(uuid);
    }
}
