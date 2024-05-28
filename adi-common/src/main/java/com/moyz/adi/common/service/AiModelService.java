package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.dto.AiModelDto;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.AiModelMapper;
import com.moyz.adi.common.util.MPPageUtil;
import dev.langchain4j.agent.tool.P;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.moyz.adi.common.util.LocalCache.MODEL_ID_TO_OBJ;
import static com.moyz.adi.common.util.LocalCache.MODEL_NAME_TO_OBJ;

@Slf4j
@Service
public class AiModelService extends ServiceImpl<AiModelMapper, AiModel> {

    @Scheduled(fixedDelay = 5 * 60 * 1000)
    public void initAll() {
        List<AiModel> all = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(AiModel::getIsDeleted, false)
                .list();
        for (AiModel model : all) {
            MODEL_NAME_TO_OBJ.put(model.getName(), model);
            MODEL_ID_TO_OBJ.put(model.getId(), model);
        }
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

    public Page<AiModelDto> page(String platform, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<AiModel> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(platform)) {
            lambdaQueryWrapper.eq(AiModel::getPlatform, platform);
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

    public void softDelete(Long id) {
        AiModel existModel = getByIdOrThrow(id);

        AiModel model = new AiModel();
        model.setId(id);
        model.setIsDeleted(true);
        baseMapper.updateById(model);

        MODEL_NAME_TO_OBJ.remove(existModel.getName());
        MODEL_ID_TO_OBJ.remove(existModel.getId());
    }
}
