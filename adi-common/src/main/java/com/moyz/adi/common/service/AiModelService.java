package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.mapper.AiModelMapper;
import lombok.extern.slf4j.Slf4j;
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

}
