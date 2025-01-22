package com.moyz.adi.chat.controller;

import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.DrawService;
import com.moyz.adi.common.service.FileService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotBlank;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.constraints.Length;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import static com.moyz.adi.common.enums.ErrorEnum.*;

/**
 * 绘图
 */
@Slf4j
@RestController
@RequestMapping("/draw")
@Validated
public class DrawController {

    @Resource
    private DrawService drawService;

    @Resource
    private FileService fileService;

    @PostMapping("/generation")
    public Map<String, String> generation(@RequestBody @Validated GenerateImageReq generateImageReq) {
        String uuid = drawService.createByPrompt(generateImageReq);
        return Map.of("uuid", uuid);
    }

    @PostMapping("/regenerate/{uuid}")
    public void regenerate(@PathVariable @Length(min = 32, max = 32) String uuid) {
        drawService.regenerate(uuid);
    }

    @Operation(summary = "Edit image")
    @PostMapping("/edit")
    public Map<String, String> edit(@RequestBody EditImageReq editImageReq) {
        String uuid = drawService.editByOriginalImage(editImageReq);
        return Map.of("uuid", uuid);
    }

    @Operation(summary = "Image variation")
    @PostMapping("/variation")
    public Map<String, String> variation(@RequestBody VariationImageReq variationImageReq) {
        String uuid = drawService.variationImage(variationImageReq);
        return Map.of("uuid", uuid);
    }

    @GetMapping("/list")
    public DrawListResp list(@RequestParam Long maxId, @RequestParam int pageSize) {
        return drawService.listByCurrentUser(maxId, pageSize);
    }


    /**
     * 获取绘图任务详情
     *
     * @param uuid 绘图任务uuid
     * @return 绘图任务详情
     */
    @GetMapping("/detail/{uuid}")
    public DrawDto getOne(@PathVariable String uuid) {
        DrawDto drawDto = drawService.getPublicOrMine(uuid);
        if (null == drawDto) {
            throw new BaseException(A_DRAW_NOT_FOUND);
        }
        return drawDto;
    }

    @GetMapping("/detail/newer-public/{uuid}")
    public DrawDto prevPublic(@PathVariable String uuid) {
        return drawService.newerPublicOne(uuid);
    }

    @GetMapping("/detail/older-public/{uuid}")
    public DrawDto nextPublic(@PathVariable String uuid) {
        return drawService.olderPublicOne(uuid);
    }

    @GetMapping("/detail/newer-starred/{uuid}")
    public DrawDto prevStarred(@PathVariable String uuid) {
        return drawService.newerStarredOne(uuid);
    }

    @GetMapping("/detail/older-starred/{uuid}")
    public DrawDto nextStarred(@PathVariable String uuid) {
        return drawService.olderStarredOne(uuid);
    }

    @GetMapping("/detail/newer-mine/{uuid}")
    public DrawDto prevMine(@PathVariable String uuid) {
        return drawService.newerMine(uuid);
    }

    @GetMapping("/detail/older-mine/{uuid}")
    public DrawDto nextMine(@PathVariable String uuid) {
        return drawService.olderMine(uuid);
    }

    /**
     * 删除绘图任务{uuid}的所有内容（提示词及生成的所有图片）
     *
     * @param uuid
     * @return
     */
    @PostMapping("/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        return drawService.del(uuid);
    }

    /**
     * 删除绘图任务{uuid}中的一张图片
     *
     * @param uuid     绘图任务的uuid
     * @param fileUuid 待删除图片uuid
     * @return
     */
    @PostMapping("/file/del/{fileUuid}")
    public boolean fileDel(@RequestParam @NotBlank String uuid, @PathVariable String fileUuid) {
        return drawService.delGeneratedFile(uuid, fileUuid);
    }

    @Operation(summary = "将绘图任务设置为公开或私有")
    @PostMapping("/set-public/{uuid}")
    public DrawDto setPublic(@PathVariable @NotBlank String uuid, @RequestParam(defaultValue = "false") Boolean isPublic, @RequestParam(required = false) Boolean withWatermark) {
        return drawService.setDrawPublic(uuid, isPublic, withWatermark);
    }

    @Operation(summary = "公开的绘图任务列表")
    @GetMapping("/public/list")
    public DrawListResp publicList(@RequestParam Long maxId, @RequestParam int pageSize) {
        return drawService.listPublic(maxId, pageSize);
    }

    @Operation(summary = "公开的图片,可能带水印（根据水印设置决定）")
    @GetMapping(value = "/public/image/{drawUuid}/{imageUuid}", produces = MediaType.IMAGE_PNG_VALUE)
    public void publicImage(@Length(min = 32, max = 32) @PathVariable String drawUuid, @Length(min = 32, max = 32) @PathVariable String imageUuid, HttpServletResponse response) {
        DrawDto drawDto = drawService.getPublicOrMine(drawUuid);
        if (null == drawDto) {
            throw new BaseException(A_AI_IMAGE_NO_AUTH);
        }
        BufferedImage bufferedImage = fileService.readImage(imageUuid, false);
        //把图片写给浏览器
        try {
            ImageIO.write(bufferedImage, "png", response.getOutputStream());
        } catch (IOException e) {
            log.error("publicImage error", e);
            throw new BaseException(B_IMAGE_LOAD_ERROR);
        }
    }

    @Operation(summary = "公开的缩略图,可能带水印（根据水印设置决定）")
    @GetMapping(value = "/public/thumbnail/{drawUuid}/{imageUuid}", produces = MediaType.IMAGE_PNG_VALUE)
    public void publicThumbnail(@Length(min = 32, max = 32) @PathVariable String drawUuid, @Length(min = 32, max = 32) @PathVariable String imageUuid, HttpServletResponse response) {
        DrawDto drawDto = drawService.getPublicOrMine(drawUuid);
        if (null == drawDto) {
            throw new BaseException(A_AI_IMAGE_NO_AUTH);
        }
        BufferedImage bufferedImage = fileService.readImage(imageUuid, true);
        try {
            ImageIO.write(bufferedImage, "png", response.getOutputStream());
        } catch (IOException e) {
            log.error("publicThumbnail error", e);
            throw new BaseException(B_IMAGE_LOAD_ERROR);
        }
    }
}
