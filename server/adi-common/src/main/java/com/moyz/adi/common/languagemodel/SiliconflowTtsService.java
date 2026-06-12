package com.moyz.adi.common.languagemodel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.languagemodel.data.TtsJobData;
import com.moyz.adi.common.languagemodel.data.SiliconflowTtsReq;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.TtsUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.nio.ByteBuffer;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

import static com.moyz.adi.common.cosntant.AdiConstant.TtsConstant.SILICONFLOW_DEFAULT_VOICE;

/**
 * 硅基流动的 TTS 服务，对应的接口不能流式输入，只能流式输出
 */
@Slf4j
public class SiliconflowTtsService extends AbstractTtsModelService {
    private final Map<String, TtsJobData> jobToData = new ConcurrentHashMap<>();

    public SiliconflowTtsService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    protected void doStart(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError) {
        if (StringUtils.isBlank(voice)) {
            voice = SILICONFLOW_DEFAULT_VOICE;
        } else {
            // 相关参数：https://docs.siliconflow.cn/cn/api-reference/audio/create-speech#cosyvoice2-0-5b
            boolean match = false;
            ObjectNode node = aiModel.getProperties();
            if (node.has("voices") && node.get("voices").isArray()) {
                ArrayNode voices = node.withArray("voices");
                for (JsonNode voiceNode : voices) {
                    if (voice.equals(voiceNode.get("name").asText())) {
                        match = true;
                        break;
                    }
                }
            }
            if (!match) {
                voice = SILICONFLOW_DEFAULT_VOICE;
            }
        }
        TtsJobData jobData = new TtsJobData();
        jobData.setVoice(voice);
        jobData.setText(new StringBuilder());
        jobData.setProcessCallback(onProcess);
        jobData.setCompleteCallback(onComplete);
        jobData.setErrorCallback(onError);
        jobToData.put(jobId, jobData);
    }

    @Override
    public void processByStream(String jobId, String partText) {
        if (null == jobToData.get(jobId)) {
            return;
        }
        jobToData.get(jobId).getText().append(partText);
    }

    @Override
    protected void doComplete(String jobId) {
        if (null == jobToData.get(jobId)) {
            return;
        }
        TtsJobData jobData = jobToData.get(jobId);
        jobToData.remove(jobId);
        SiliconflowTtsReq.SiliconflowTtsReqBuilder requestBuilder = SiliconflowTtsReq.builder()
                .input(jobData.getText().toString())
                .model(aiModel.getName())
                .voice(jobData.getVoice())
                .stream(true);
        if (StringUtils.isNotBlank(jobData.getVoice())) {
            requestBuilder.voice(jobData.getVoice());
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Authorization", "Bearer " + platform.getApiKey());
        String url = platform.getBaseUrl() + "/audio/speech";
        String modelName = aiModel.getName().toLowerCase().replace("/", "-");

        TtsUtil.executeAndSave(url, headers, JsonUtil.toJson(requestBuilder.build()).getBytes(),
                modelName, jobData.getProcessCallback(), jobData.getCompleteCallback());
    }
}
