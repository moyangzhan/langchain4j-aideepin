package com.moyz.adi.chat.controller;

import com.moyz.adi.common.service.FileService;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.validator.constraints.Length;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@Validated
public class FileController {

    @Resource
    private FileService fileService;

    @GetMapping(value = "/image/{uuid}", produces = MediaType.IMAGE_PNG_VALUE)
    public void image(@Length(min = 32, max = 32) @PathVariable String uuid, HttpServletResponse response) {
        BufferedImage bufferedImage = fileService.readBufferedImage(uuid);
        //把图片写给浏览器
        try {
            ImageIO.write(bufferedImage, "png", response.getOutputStream());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @PostMapping(path = "/file/upload", headers = "content-type=multipart/form-data", produces = MediaType.APPLICATION_JSON_VALUE)
    public Map<String, String> upload(@RequestPart(value = "file") MultipartFile file) {
        Map<String, String> result = new HashMap<>();
        result.put("uuid", fileService.writeToLocal(file));
        return result;
    }

    @PostMapping("/file/del/{uuid}")
    public boolean del(@PathVariable String uuid) {
        return fileService.softDel(uuid);
    }
}
