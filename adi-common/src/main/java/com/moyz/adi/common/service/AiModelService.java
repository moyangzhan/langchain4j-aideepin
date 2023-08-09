package com.moyz.adi.common.service;

import com.moyz.adi.common.enums.AiModelStatus;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.helper.OpenAiHelper;
import com.moyz.adi.common.mapper.AiModelMapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class AiModelService extends ServiceImpl<AiModelMapper, AiModel> {

    public final static List<AiModel> AI_MODELS = new ArrayList<>();

    @Resource
    private OpenAiHelper openAiHelper;

    @PostConstruct
    public void init() {
        List<AiModel> aiModels = this.lambdaQuery().eq(AiModel::getModelStatus, AiModelStatus.ACTIVE).list();
        AI_MODELS.addAll(aiModels);

        //get models from openai
//        List<Model> openaiModels = openAiHelper.getModels();
//        for (Model model : openaiModels) {
//            AiModel aiModel = this.lambdaQuery().eq(AiModel::getName, model.getId()).one();
//            if (null == aiModel) {
//                aiModel = new AiModel();
//                aiModel.setName(model.getId());
//                aiModel.setModelStatus(AiModelStatus.INACTIVE);
//                baseMapper.insert(aiModel);
//            }
//        }
        //refresh models cache
//        aiModels = this.lambdaQuery().eq(AiModel::getModelStatus, AiModelStatus.ACTIVE).list();
//        AI_MODELS.clear();
//        AI_MODELS.addAll(aiModels);
    }
}
