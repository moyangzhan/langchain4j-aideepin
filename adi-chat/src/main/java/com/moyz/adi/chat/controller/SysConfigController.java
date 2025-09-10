package com.moyz.adi.chat.controller;

import com.aliyun.core.utils.StringUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.AiModelService;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AsrSetting;
import com.moyz.adi.common.vo.ModelVoice;
import com.moyz.adi.common.vo.SysConfigResp;
import com.moyz.adi.common.vo.TtsSetting;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@RestController
@RequestMapping("/sys/config")
@Validated
public class SysConfigController {

    @Resource
    private AiModelService aiModelService;

    @GetMapping(value = "/public/info")
    public SysConfigResp info() {
        String asrSetting = LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.ASR_SETTING);
        String ttsSetting = LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.TTS_SETTING);
        if (StringUtils.isBlank(asrSetting)) {
            throw new BaseException(B_ASR_SETTING_NOT_FOUND);
        }
        if (StringUtils.isBlank(ttsSetting)) {
            throw new BaseException(B_TTS_SETTING_NOT_FOUND);
        }
        TtsSetting tts = JsonUtil.fromJson(ttsSetting, TtsSetting.class);
        if (null == tts) {
            throw new BaseException(B_TTS_SETTING_NOT_FOUND);
        }

        //Available voices depend on the modelName in ttsSetting
        AiModel aiModel = aiModelService.getByNameOrThrow(tts.getModelName());
        if (null == aiModel) {
            log.error("Model {} not found in db", tts.getModelName());
            throw new BaseException(B_TTS_MODEL_NOT_FOUND);
        }
        List<ModelVoice> voices = new ArrayList<>();
        JsonNode modelVoices = aiModel.getProperties().get("voices");
        for (JsonNode voice : modelVoices) {
            voices.add(JsonUtil.fromJson(voice, ModelVoice.class));
        }
        SysConfigResp sysConfigResp = new SysConfigResp();
        sysConfigResp.setAsrSetting(JsonUtil.fromJson(asrSetting, AsrSetting.class));
        sysConfigResp.setTtsSetting(JsonUtil.fromJson(ttsSetting, TtsSetting.class));
        sysConfigResp.setAvailableVoices(voices);
        return sysConfigResp;
    }
}
