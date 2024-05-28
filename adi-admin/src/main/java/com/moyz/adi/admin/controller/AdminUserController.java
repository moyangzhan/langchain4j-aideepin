package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.UserAddReq;
import com.moyz.adi.common.dto.UserEditReq;
import com.moyz.adi.common.dto.UserInfoDto;
import com.moyz.adi.common.dto.UsersReq;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.service.UserService;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.cat.CatAvatar;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.BeanUtils;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;

@RestController
@RequestMapping("/admin/user")
@Validated
public class AdminUserController {

    @Resource
    private UserService userService;

    @PostMapping("/list")
    public Page<UserInfoDto> userList(@RequestBody UsersReq usersReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return userService.listUsers(usersReq, currentPage, pageSize);
    }

    @Operation(summary = "用户信息")
    @GetMapping("/info/{uuid}")
    public UserInfoDto info(@PathVariable String uuid) {
        User user = userService.getByUuidOrThrow(uuid);
        UserInfoDto result = new UserInfoDto();
        BeanUtils.copyProperties(user, result);
        return result;
    }

    @PostMapping("/addOne")
    public UserInfoDto addOne(@Validated @RequestBody UserAddReq addUserReq) {
        return userService.addUser(addUserReq);
    }

    @PostMapping("/active/{uuid}")
    public void activeByUuid(@PathVariable String uuid) {
        userService.activeByUuid(uuid);
    }

    @PostMapping("/freeze/{uuid}")
    public void freezeByUuid(@PathVariable String uuid) {
        userService.freeze(uuid);
    }

    @PostMapping("/edit")
    public void editUser(@Validated @RequestBody UserEditReq userEditReq) {
        userService.editUser(userEditReq);
    }
}
