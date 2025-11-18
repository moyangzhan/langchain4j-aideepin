package com.moyz.adi.common.languagemodel;

import com.fasterxml.jackson.databind.JsonNode;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.file.LocalFileUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.Consts;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.DefaultHttpRequestRetryHandler;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import java.io.File;
import java.io.IOException;

import static com.moyz.adi.common.cosntant.AdiConstant.FORM_DATA_BOUNDARY_PRE;
import static org.springframework.http.HttpHeaders.AUTHORIZATION;
import static org.springframework.http.HttpHeaders.CONTENT_TYPE;

@Slf4j
public class SiliconflowAsrService extends AbstractAsrModelService {

    public SiliconflowAsrService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public String audioToText(String urlOrPath) {
        String audioPath = urlOrPath;
        if (audioPath.indexOf("http") == 0) {
            audioPath = LocalFileUtil.saveFromUrl(audioPath, UuidUtil.createShort(), "wav");
        }
        log.info("audio to text,path:{}", audioPath);
        String baseUrl = platform.getBaseUrl();
        if (StringUtils.isBlank(baseUrl)) {
            baseUrl = "https://api.siliconflow.cn";
        }
        RequestConfig requestConfig = RequestConfig.custom()
                .setConnectionRequestTimeout(30 * 1000)
                .setConnectTimeout(30 * 1000)
                .build();
        try (CloseableHttpClient httpClient = HttpClients.custom()
                .setDefaultRequestConfig(requestConfig)
                .setRetryHandler(new DefaultHttpRequestRetryHandler(0, true))
                .build()) {
            String boundary = FORM_DATA_BOUNDARY_PRE + System.currentTimeMillis();
            HttpPost httpRequest = new HttpPost(baseUrl + "/audio/transcriptions");
            httpRequest.addHeader(CONTENT_TYPE, "multipart/form-data; boundary=" + boundary);
            httpRequest.addHeader(AUTHORIZATION, "Bearer " + platform.getApiKey());
            MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
            entityBuilder.setBoundary(boundary);
            entityBuilder.addPart("file", new FileBody(new File(audioPath)));
            entityBuilder.addTextBody("model", aiModel.getName());
            httpRequest.setEntity(entityBuilder.build());
            try (CloseableHttpResponse response = httpClient.execute(httpRequest)) {
                int statusCode = response.getStatusLine().getStatusCode();
                String responseBody = EntityUtils.toString(response.getEntity(), Consts.UTF_8);
                log.info("ASR response: {}", responseBody);
                if (statusCode == 200) {
                    JsonNode jsonNode = JsonUtil.toJsonNode(responseBody);
                    if (null == jsonNode) {
                        log.error("ASR response is not a valid JSON: {}", responseBody);
                        return null;
                    }
                    if (jsonNode.has("text")) {
                        String text = jsonNode.get("text").asText();
                        log.info("ASR Result: {}", text);
                        return text;
                    } else {
                        log.error("ASR response does not contain 'text': {}", responseBody);
                    }
                } else {
                    log.error("请求失败:{}", response);
                }
            }
        } catch (IOException e) {
            log.error("请求失败:{}", e.getMessage());
        }
        return null;
    }
}
