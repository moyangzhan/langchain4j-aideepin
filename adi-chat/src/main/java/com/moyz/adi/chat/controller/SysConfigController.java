package com.moyz.adi.chat.controller;

import com.aliyun.core.utils.StringUtils;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AsrSetting;
import com.moyz.adi.common.vo.SysConfigResp;
import com.moyz.adi.common.vo.TtsSetting;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static com.moyz.adi.common.enums.ErrorEnum.*;

@RestController
@RequestMapping("/sys/config")
@Validated
public class SysConfigController {

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
        SysConfigResp sysConfigResp = new SysConfigResp();
        sysConfigResp.setAsrSetting(JsonUtil.fromJson(asrSetting, AsrSetting.class));
        sysConfigResp.setTtsSetting(JsonUtil.fromJson(ttsSetting, TtsSetting.class));
        return sysConfigResp;
    }
}
