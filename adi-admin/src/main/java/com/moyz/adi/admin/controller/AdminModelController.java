package com.moyz.adi.admin.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.dto.AiModelSearchReq;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AiModelAddGroup;
import com.moyz.adi.common.interfaces.AiModelEditGroup;
import com.moyz.adi.common.service.AiModelService;
import com.moyz.adi.common.util.AiModelUtil;
import jakarta.annotation.Resource;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;

@RestController
@RequestMapping("/admin/model")
@Validated
public class AdminModelController {

    @Resource
    private AiModelService aiModelService;

    @PostMapping("/search")
    public Page<AiModelDto> page(@RequestBody AiModelSearchReq aiModelSearchReq, @NotNull @Min(1) Integer currentPage, @NotNull @Min(10) Integer pageSize) {
        return aiModelService.search(aiModelSearchReq, currentPage, pageSize);
    }

    @PostMapping("/addOne")
    public AiModelDto addOne(@Validated(AiModelAddGroup.class) @RequestBody AiModelDto aiModelDto) {
        check(aiModelDto.getPlatform(), aiModelDto.getType());
        return aiModelService.addOne(aiModelDto);
    }

    @PostMapping("/edit")
    public void edit(@Validated(AiModelEditGroup.class) @RequestBody AiModelDto aiModelDto) {
        check(aiModelDto.getPlatform(), aiModelDto.getType());
        aiModelService.edit(aiModelDto);
    }

    @PostMapping("/delete/{id}")
    public void delete(@PathVariable Long id) {
        aiModelService.softDelete(id);
    }

    private void check(String platform, String type) {
        if (StringUtils.isNotBlank(platform) && !AiModelUtil.checkModelPlatform(platform)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        if (StringUtils.isNotBlank(type) && !AiModelUtil.checkModelType(type)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
    }
}
