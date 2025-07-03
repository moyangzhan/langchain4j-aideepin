package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;

import java.net.InetSocketAddress;

public abstract class AbstractAsrModelService<T> extends CommonModelService<T> {

    protected AbstractAsrModelService(AiModel aiModel, String settingName, Class<T> clazz) {
        super(aiModel, settingName, clazz);
    }

    public abstract String audioToText(String urlOrPath);

    public AbstractAsrModelService<T> setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }
}
