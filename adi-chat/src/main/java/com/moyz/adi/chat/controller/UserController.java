package com.moyz.adi.chat.controller;

import com.moyz.adi.common.service.UserService;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ConfigResp;
import com.moyz.adi.common.dto.ModifyPasswordReq;
import com.moyz.adi.common.dto.UserUpdateReq;
import com.moyz.adi.common.entity.User;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.cat.CatAvatar;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.MixedUnits;
import org.hibernate.validator.constraints.Length;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;

@Slf4j
@Tag(name = "用户controller")
@RestController
@RequestMapping("/user")
public class UserController {

    @Resource
    private UserService userService;

    @Operation(summary = "用户信息")
    @GetMapping("/{uuid}")
    public void info(@Validated @PathVariable String uuid) {
        log.info(uuid);
    }

    @Operation(summary = "配置信息")
    @GetMapping("/config")
    public ConfigResp configInfo() {
        return userService.getConfig();
    }

    @Operation(summary = "更新信息")
    @PostMapping("/edit")
    public void update(@Validated UserUpdateReq userUpdateReq) {
        userService.updateConfig(userUpdateReq);
    }

    @Operation(summary = "修改密码")
    @PostMapping("/password/modify")
    public String modifyPassword(@RequestBody ModifyPasswordReq modifyPasswordReq) {
        userService.modifyPassword(modifyPasswordReq.getOldPassword(), modifyPasswordReq.getNewPassword());
        return "修改成功";
    }

    @Operation(summary = "退出")
    @PostMapping("/logout")
    public void logout() {
        userService.logout();
    }

    @Operation(summary = "当前用户头像")
    @GetMapping(value = "/myAvatar", produces = MediaType.IMAGE_JPEG_VALUE)
    public void myAvatar(HttpServletResponse response) {
        User user = ThreadContext.getCurrentUser();
        Avatar avatar = CatAvatar.newAvatarBuilder().build();
        BufferedImage bufferedImage = avatar.create(user.getId());
        //把图片写给浏览器
        try {
            ImageIO.write(bufferedImage, "png", response.getOutputStream());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Operation(summary = "用户头像")
    @GetMapping(value = "/avatar/{uuid}", produces = MediaType.IMAGE_JPEG_VALUE)
    public void avatar(@Validated @PathVariable String uuid, @RequestParam(defaultValue = "64") @Min(32) @Max(128) Integer width, @RequestParam(defaultValue = "64") @Min(32) @Max(128) Integer height, HttpServletResponse response) {
        User user = userService.getByUuid(uuid);
        long userId = 0;
        if (null != user) {
            userId = user.getId();
        }
        try {
            writeToResponse(userId, width, height, response);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private synchronized void writeToResponse(Long userId, Integer width, Integer height, HttpServletResponse response) throws IOException {
        response.setHeader("Cache-Control", "max-age=" + 3600 * 24 * 7);
        // 设置Expires头，指示响应过期的具体日期时间
        long expiresTime = System.currentTimeMillis() + 3600 * 24 * 7 * 1000;
        response.setDateHeader("Expires", expiresTime);
        Avatar avatar = CatAvatar.newAvatarBuilder().size(width, height).build();
        BufferedImage bufferedImage = avatar.create(userId);
        ImageIO.write(bufferedImage, "png", response.getOutputStream());
    }
}
