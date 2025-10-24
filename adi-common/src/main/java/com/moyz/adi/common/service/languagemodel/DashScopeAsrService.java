package com.moyz.adi.common.service.languagemodel;

import com.alibaba.dashscope.audio.asr.transcription.*;
import com.alibaba.dashscope.common.TaskStatus;
import com.aliyun.oss.common.utils.StringUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.SpringUtil;
import com.moyz.adi.common.vo.DashScopeSetting;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static com.moyz.adi.common.enums.ErrorEnum.B_URL_INVALID;

@Slf4j
public class DashScopeAsrService extends AbstractAsrModelService {

    public DashScopeAsrService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public String audioToText(String urlOrPath) {
        //只支持远程URL
        if (!StringUtils.beginsWithIgnoreCase(urlOrPath, "http")) {
            log.info("Audio path is not a URL: {}", urlOrPath);
            throw new BaseException(B_URL_INVALID);
        }
        TranscriptionParam param = TranscriptionParam.builder()
                .apiKey(platform.getApiKey())
                .model(aiModel.getName())
                // “language_hints”只支持paraformer-v2模型，指定中文、粤语、英文
                .parameter("language_hints", new String[]{"zh", "yue", "en"})
                .fileUrls(Collections.singletonList(urlOrPath))
                .build();
        try {
            Transcription transcription = new Transcription();
            TranscriptionResult result = transcription.asyncCall(param);
            log.info("RequestId: " + result.getRequestId());
            TranscriptionQueryParam queryParam = TranscriptionQueryParam.FromTranscriptionParam(param, result.getTaskId());
            TranscriptionResult finalResult = transcription.wait(queryParam);
            if (TaskStatus.SUCCEEDED == finalResult.getTaskStatus()) {
                StringBuilder sb = new StringBuilder();
                List<TranscriptionTaskResult> results = finalResult.getResults();
                for (TranscriptionTaskResult transcriptionTaskResult : results) {
                    if (transcriptionTaskResult.getSubTaskStatus() == TaskStatus.FAILED) {
                        log.error("Transcription task failed: {}", transcriptionTaskResult.getMessage());
                        continue;
                    }
                    JsonNode jsonNode = SpringUtil.getBean(RestTemplate.class).getForObject(new URI(transcriptionTaskResult.getTranscriptionUrl()), JsonNode.class);
                    JsonNode transcripts = jsonNode.get("transcripts");
                    if (null == transcripts) {
                        log.error("Failed to parse transcription JSON: {}", jsonNode);
                        continue;
                    }
                    for (JsonNode transcript : transcripts) {
                        String text = transcript.get("text").asText();
                        sb.append(text);
                    }
                }
                return sb.toString();
            } else {
                log.error("Transcription failed: {}", finalResult.getTaskStatus());
            }
        } catch (Exception e) {
            log.error("Error during transcription: {}", e.getMessage());
        }
        return null;
    }
}
