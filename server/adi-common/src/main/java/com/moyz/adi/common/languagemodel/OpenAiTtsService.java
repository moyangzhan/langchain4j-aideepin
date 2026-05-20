package com.moyz.adi.common.languagemodel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.languagemodel.data.OpenAiTtsReq;
import com.moyz.adi.common.languagemodel.data.TtsJobData;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.TtsUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static com.moyz.adi.common.cosntant.AdiConstant.TtsConstant.OPENAI_DEFAULT_VOICE;

@Slf4j
public class OpenAiTtsService extends AbstractTtsModelService {

    private final Map<String, TtsJobData> jobToData = new HashMap<>();

    public OpenAiTtsService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public void start(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError) {
        if (StringUtils.isBlank(voice)) {
            voice = OPENAI_DEFAULT_VOICE;
        } else {
            boolean match = false;
            ObjectNode node = aiModel.getProperties();
            if (node != null && node.has("voices") && node.get("voices").isArray()) {
                ArrayNode voices = node.withArray("voices");
                for (JsonNode voiceNode : voices) {
                    if (voice.equals(voiceNode.get("param_name").asText())) {
                        match = true;
                        break;
                    }
                }
            }
            if (!match) {
                voice = OPENAI_DEFAULT_VOICE;
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
    public void complete(String jobId) {
        if (null == jobToData.get(jobId)) {
            return;
        }
        TtsJobData jobData = jobToData.get(jobId);
        jobToData.remove(jobId);

        var requestBuilder = OpenAiTtsReq.builder()
                .input(jobData.getText().toString())
                .model(aiModel.getName())
                .voice(jobData.getVoice())
                .responseFormat("mp3");

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Authorization", "Bearer " + platform.getApiKey());
        String baseUrl = platform.getBaseUrl();
        if (StringUtils.isBlank(baseUrl)) {
            baseUrl = "https://api.openai.com/v1";
        }
        String url = baseUrl + "/audio/speech";
        String modelName = aiModel.getName().toLowerCase();

        TtsUtil.executeAndSave(url, headers, JsonUtil.toJson(requestBuilder.build()).getBytes(),
                modelName, jobData.getProcessCallback(), jobData.getCompleteCallback());
    }
}
