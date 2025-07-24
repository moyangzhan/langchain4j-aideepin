package com.moyz.adi.common.helper;

import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.service.languagemodel.AbstractTtsModelService;
import com.moyz.adi.common.util.JsonUtil;
import lombok.extern.slf4j.Slf4j;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

@Slf4j
public class TtsModelContext {

    //ModelName to TTS service mapping
    private static final Map<String, AbstractTtsModelService<?>> NAME_TO_SERVICE = new HashMap<>();

    private final AbstractTtsModelService<?> current;

    /**
     * 直接由系统设置来决定使用哪个TTS模型，不需要让用户选择。
     */
    public TtsModelContext() {
        String asrSetting = SysConfigService.getByKey(AdiConstant.SysConfigKey.TTS_SETTING);
        log.info("tts model setting:{}", asrSetting);
        JsonNode jsonNode = JsonUtil.toJsonNode(asrSetting);
        if (null != jsonNode) {
            String modelName = jsonNode.get("model_name").asText();
            this.current = NAME_TO_SERVICE.get(modelName);
            if (null == this.current) {
                log.error("asr model not found,modelName:{}", modelName);
                throw new BaseException(ErrorEnum.B_TTS_MODEL_NOT_FOUND);
            }
        } else {
            throw new BaseException(ErrorEnum.B_TTS_SETTING_NOT_FOUND);
        }
    }

    public static void addService(AbstractTtsModelService<?> modelService) {
        NAME_TO_SERVICE.put(modelService.getAiModel().getName(), modelService);
    }

    public static void clearByPlatform(String platform) {
        List<String> needDeleted = NAME_TO_SERVICE.values()
                .stream()
                .filter(item -> item.getAiModel().getPlatform().equalsIgnoreCase(platform))
                .map(item -> item.getAiModel().getName())
                .toList();
        for (String key : needDeleted) {
            log.info("delete tts model service,modelName:{}", key);
            NAME_TO_SERVICE.remove(key);
        }
    }

    /**
     * 开启一个TTS任务
     *
     * @param jobId      任务ID
     * @param voice      声音
     * @param onProcess  处理回调
     * @param onComplete 完成回调
     * @param onError    异常回调
     */
    public void startTtsJob(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError) {
        log.info("start tts job,jobId:{},voice:{}", jobId, voice);
        current.start(jobId, voice, onProcess, onComplete, onError);
    }

    /**
     * 处理文本
     *
     * @param jobId 任务id
     * @param text  文本内容
     */
    public void processPartialText(String jobId, String text) {
        current.processByStream(jobId, text);
    }

    /**
     * 主动完成TTS任务，会触发onComplete回调
     *
     * @param jobId 任务id
     */
    public void complete(String jobId) {
        current.complete(jobId);
    }

}
