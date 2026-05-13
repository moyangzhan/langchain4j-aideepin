package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.ModelPlatformSearchReq;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.mapper.ModelPlatformMapper;
import com.moyz.adi.common.util.MPPageUtil;
import io.micrometer.common.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ModelPlatformService extends ServiceImpl<ModelPlatformMapper, ModelPlatform> {
    public List<ModelPlatform> listAll() {
        return this.lambdaQuery().eq(ModelPlatform::getIsDeleted, false).list();
    }

    public Page<ModelPlatform> search(ModelPlatformSearchReq searchReq, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<ModelPlatform> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.like(StringUtils.isNotBlank(searchReq.getName()), ModelPlatform::getName, searchReq.getName());
        lambdaQueryWrapper.like(StringUtils.isNotBlank(searchReq.getTitle()), ModelPlatform::getTitle, searchReq.getTitle());
        lambdaQueryWrapper.eq(ModelPlatform::getIsDeleted, false);
        lambdaQueryWrapper.orderByDesc(ModelPlatform::getUpdateTime);
        Page<ModelPlatform> aiModelPage = baseMapper.selectPage(new Page<>(currentPage, pageSize), lambdaQueryWrapper);
        return MPPageUtil.convertToPage(aiModelPage, new Page<>(), ModelPlatform.class);
    }

    public ModelPlatform getByName(String name) {
        LambdaQueryWrapper<ModelPlatform> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper
                .eq(ModelPlatform::getName, name)
                .eq(ModelPlatform::getIsDeleted, false);
        return baseMapper.selectOne(lambdaQueryWrapper);
    }

    public ModelPlatform getById(Long id) {
        LambdaQueryWrapper<ModelPlatform> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper
                .eq(ModelPlatform::getId, id)
                .eq(ModelPlatform::getIsDeleted, false);
        return baseMapper.selectOne(lambdaQueryWrapper);
    }

    public ModelPlatform addOne(ModelPlatform modelPlatform) {
        if (!ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        ModelPlatform exist = this.getByName(modelPlatform.getName());
        if (null != exist) {
            throw new IllegalArgumentException("Model platform with name " + modelPlatform.getName() + " already exists.");
        }
        ModelPlatform newObj = new ModelPlatform();
        BeanUtils.copyProperties(modelPlatform, newObj);
        newObj.setId(null);
        newObj.setCreateTime(null);
        newObj.setUpdateTime(null);
        newObj.setIsDeleted(null);
        this.save(modelPlatform);
        return this.getByName(modelPlatform.getName());
    }

    public ModelPlatform edit(ModelPlatform modelPlatform) {
        if (!ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        ModelPlatform exist = this.getById(modelPlatform.getId());
        if (null == exist) {
            throw new IllegalArgumentException("Model platform with id " + modelPlatform.getId() + " does not exist.");
        }
        exist.setCreateTime(null);
        exist.setUpdateTime(null);
        BeanUtils.copyProperties(modelPlatform, exist);
        this.updateById(exist);
        ModelPlatform newPlatform = this.getById(modelPlatform.getId());

        //Update all related LLMService platform info
        LLMContext.getAllServices().forEach(service -> {
            if (service.getPlatform().getName().equals(newPlatform.getName())) {
                service.setPlatform(newPlatform);
            }
        });
        return newPlatform;
    }

    public ModelPlatform getByIdOrThrow(Long id) {
        ModelPlatform exist = baseMapper.selectById(id);
        if (null == exist) {
            throw new BaseException(ErrorEnum.A_MODEL_NOT_FOUND);
        }
        return exist;
    }

    public void softDelete(Long id) {
        if (!ThreadContext.getCurrentUser().getIsAdmin()) {
            throw new BaseException(ErrorEnum.A_USER_NOT_AUTH);
        }
        ModelPlatform updateObj = new ModelPlatform();
        updateObj.setId(id);
        updateObj.setIsDeleted(true);
        baseMapper.updateById(updateObj);
    }
}
