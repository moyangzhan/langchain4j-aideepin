package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.dto.AiModelSearchReq;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.AiModelMapper;
import com.moyz.adi.common.util.MPPageUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class AiModelService extends ServiceImpl<AiModelMapper, AiModel> {

    @Resource
    private AiModelSettingService aiModelSettingService;

    public void init() {
        List<AiModel> aiModels = ChainWrappers.lambdaQueryChain(baseMapper).eq(AiModel::getIsDeleted, false).list();
        aiModelSettingService.init(aiModels);
    }

    public List<AiModel> listBy(String platform, String type) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiModel::getPlatform, platform)
                .eq(AiModel::getType, type)
                .eq(AiModel::getIsDeleted, false)
                .list();
    }

    public AiModel getByName(String modelName) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiModel::getName, modelName)
                .eq(AiModel::getIsDeleted, false)
                .one();
    }

    public Long getIdByName(String modelName) {
        AiModel aiModel = this.getByName(modelName);
        return null == aiModel ? 0l : aiModel.getId();
    }

    public AiModel getByIdOrThrow(Long id) {
        AiModel existModel = baseMapper.selectById(id);
        if (null == existModel) {
            throw new BaseException(ErrorEnum.A_MODEL_NOT_FOUND);
        }
        return existModel;
    }

    public Page<AiModelDto> search(AiModelSearchReq aiModelSearchReq, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<AiModel> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(aiModelSearchReq.getPlatform())) {
            lambdaQueryWrapper.eq(AiModel::getPlatform, aiModelSearchReq.getPlatform());
        }
        if (StringUtils.isNotBlank(aiModelSearchReq.getType())) {
            lambdaQueryWrapper.eq(AiModel::getType, aiModelSearchReq.getType());
        }
        if (null != aiModelSearchReq.getIsEnable()) {
            lambdaQueryWrapper.eq(AiModel::getIsEnable, aiModelSearchReq.getIsEnable());
        }
        lambdaQueryWrapper.eq(AiModel::getIsDeleted, false);
        lambdaQueryWrapper.orderByDesc(AiModel::getUpdateTime);
        Page<AiModel> aiModelPage = baseMapper.selectPage(new Page<>(currentPage, pageSize), lambdaQueryWrapper);
        return MPPageUtil.convertToPage(aiModelPage, new Page<>(), AiModelDto.class);
    }

    public void disable(Long id) {
        AiModel model = new AiModel();
        model.setId(id);
        model.setIsEnable(false);
        baseMapper.updateById(model);
    }

    public void enable(Long id) {
        AiModel model = new AiModel();
        model.setId(id);
        model.setIsEnable(true);
        baseMapper.updateById(model);
    }

    public List<AiModelDto> listEnable() {
        List<AiModel> aiModels = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiModel::getIsEnable, true)
                .eq(AiModel::getIsDeleted, false)
                .list();
        return MPPageUtil.convertToList(aiModels, AiModelDto.class);
    }

    public AiModelDto addOne(AiModelDto aiModelDto) {
        Long count = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiModel::getName, aiModelDto.getName())
                .eq(AiModel::getPlatform, aiModelDto.getPlatform())
                .eq(AiModel::getIsDeleted, false)
                .count();
        if (count > 0) {
            throw new BaseException(ErrorEnum.A_MODEL_ALREADY_EXIST);
        }
        AiModel aiModel = new AiModel();
        BeanUtils.copyProperties(aiModelDto, aiModel);
        baseMapper.insert(aiModel);

        AiModelDto result = new AiModelDto();
        BeanUtils.copyProperties(aiModel, result);

        aiModelSettingService.add(aiModel);

        return result;
    }

    public void edit(AiModelDto aiModelDto) {
        AiModel oldAiModel = getByIdOrThrow(aiModelDto.getId());

        AiModel aiModel = new AiModel();
        BeanUtils.copyProperties(aiModelDto, aiModel, "createTime", "updateTime");
        baseMapper.updateById(aiModel);

        AiModel updatedOne = getByIdOrThrow(aiModelDto.getId());
        aiModelSettingService.delete(oldAiModel);
        aiModelSettingService.add(updatedOne);
    }

    public void softDelete(Long id) {
        AiModel existModel = getByIdOrThrow(id);

        AiModel model = new AiModel();
        model.setId(id);
        model.setIsDeleted(true);
        baseMapper.updateById(model);

        aiModelSettingService.delete(existModel);
    }
}
