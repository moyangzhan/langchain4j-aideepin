package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.service.AiImageService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.NotBlank;
import org.hibernate.validator.constraints.Length;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * 作图
 */
@RestController
@RequestMapping("/ai-image")
@Validated
public class AiImageController {

    @Resource
    private AiImageService aiImageService;

    @PostMapping("/generation")
    public Map<String, String> generation(@RequestBody @Validated GenerateImageReq generateImageReq) {
        String uuid = aiImageService.createByPrompt(generateImageReq);
        return Map.of("uuid", uuid);
    }

    @PostMapping("/regenerate/{uuid}")
    public void regenerate(@PathVariable @Length(min = 32, max = 32) String uuid) {
        aiImageService.regenerate(uuid);
    }

    @Operation(summary = "Edit image")
    @PostMapping("/edit")
    public Map<String, String> edit(@RequestBody EditImageReq editImageReq) {
        String uuid = aiImageService.editByOriginalImage(editImageReq);
        return Map.of("uuid", uuid);
    }

    @Operation(summary = "Image variation")
    @PostMapping("/variation")
    public Map<String, String> variation(@RequestBody VariationImageReq variationImageReq) {
        String uuid = aiImageService.variationImage(variationImageReq);
        return Map.of("uuid", uuid);
    }

    @GetMapping("/list")
    public AiImagesListResp list(@RequestParam Long maxId, @RequestParam int pageSize) {
        return aiImageService.listAll(maxId, pageSize);
    }

    @GetMapping("/detail/{uuid}")
    public AiImageDto getOne(@PathVariable String uuid) {
        return aiImageService.getOne(uuid);
    }

    /**
     * 删除作图任务{uuid}的所有内容（提示词及生成的所有图片）
     *
     * @param uuid
     * @return
     */
    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        return aiImageService.del(uuid);
    }

    /**
     * 删除作图任务{uuid}中的一张图片
     *
     * @param uuid 作图任务的uuid
     * @param fileUuid    待删除图片uuid
     * @return
     */
    @PostMapping("/file/del/{fileUuid}")
    public boolean fileDel(@RequestParam @NotBlank String uuid, @PathVariable String fileUuid) {
        return aiImageService.delGeneratedFile(uuid, fileUuid);
    }
}
