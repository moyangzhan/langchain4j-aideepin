package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import lombok.Getter;

import java.net.InetSocketAddress;

public class CommonModelService<T> {
    protected InetSocketAddress proxyAddress;
    @Getter
    protected AiModel aiModel;
    protected T platformSetting;

    public CommonModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        this.aiModel = aiModel;
        String st = LocalCache.CONFIGS.get(settingName);
        platformSetting = JsonUtil.fromJson(st, clazz);
    }
}
