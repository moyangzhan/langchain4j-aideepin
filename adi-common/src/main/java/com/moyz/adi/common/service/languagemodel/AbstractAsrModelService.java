package com.moyz.adi.common.service.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;

import java.net.InetSocketAddress;

public abstract class AbstractAsrModelService extends CommonModelService {

    protected AbstractAsrModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    public abstract String audioToText(String urlOrPath);

    public AbstractAsrModelService setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }
}
