package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.function.Consumer;

public abstract class AbstractTtsModelService extends CommonModelService {
    public AbstractTtsModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    public abstract void start(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError);

    /**
     * 处理语音合成任务
     *
     * @param jobId    任务id
     * @param partText 文本（部分）
     */
    public abstract void processByStream(String jobId, String partText);

    public abstract void complete(String jobId);

    public AbstractTtsModelService setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }
}
