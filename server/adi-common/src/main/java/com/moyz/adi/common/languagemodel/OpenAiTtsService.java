package com.moyz.adi.common.languagemodel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.languagemodel.data.OpenAiTtsReq;
import com.moyz.adi.common.languagemodel.data.TtsJobData;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.web.client.ResponseExtractor;
import org.springframework.web.client.RestTemplate;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static com.moyz.adi.common.cosntant.AdiConstant.TtsConstant.OPENAI_DEFAULT_VOICE;
import static com.moyz.adi.common.util.LocalDateTimeUtil.PATTERN_YYYYMMDDMMHHSS;

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
        RestTemplate restTemplate = new RestTemplate();
        String baseUrl = platform.getBaseUrl();
        if (StringUtils.isBlank(baseUrl)) {
            baseUrl = "https://api.openai.com/v1";
        }
        String url = baseUrl + "/audio/speech";

        ResponseExtractor<Void> responseExtractor = response -> {
            try (InputStream inputStream = response.getBody()) {
                String datetime = LocalDateTimeUtil.format(LocalDateTime.now(), PATTERN_YYYYMMDDMMHHSS);
                String fileName = aiModel.getName().toLowerCase() + "-" + datetime + "-" + UuidUtil.createShort().substring(0, 6) + ".mp3";
                Path tempFile = Files.createTempFile("tmp-tts-" + datetime, ".mp3");
                ByteArrayOutputStream sendBuffer = new ByteArrayOutputStream();
                final int MIN_BUFFER_SIZE = 16 * 1024;
                try (OutputStream outputStream = Files.newOutputStream(tempFile);
                     BufferedOutputStream bufferedOutput = new BufferedOutputStream(outputStream)) {
                    byte[] buffer = new byte[2048];
                    int bytesRead;
                    while ((bytesRead = inputStream.read(buffer)) != -1) {
                        bufferedOutput.write(buffer, 0, bytesRead);
                        sendBuffer.write(buffer, 0, bytesRead);
                        if (sendBuffer.size() >= MIN_BUFFER_SIZE) {
                            jobData.getProcessCallback().accept(ByteBuffer.wrap(sendBuffer.toByteArray()));
                            sendBuffer.reset();
                        }
                    }
                    if (sendBuffer.size() > 0) {
                        jobData.getProcessCallback().accept(ByteBuffer.wrap(sendBuffer.toByteArray()));
                    }
                }
                Pair<String, String> pair = new FileOperatorContext().save(Files.readAllBytes(tempFile), false, fileName);
                log.info("File saved successfully, path: {}", pair.getLeft());
                jobData.getCompleteCallback().accept(pair.getLeft());
                Files.deleteIfExists(tempFile);
            }
            return null;
        };
        restTemplate.execute(url, HttpMethod.POST, requestCallback -> {
            requestCallback.getHeaders().putAll(headers);
            requestCallback.getBody().write(JsonUtil.toJson(requestBuilder.build()).getBytes());
        }, responseExtractor);
    }
}
