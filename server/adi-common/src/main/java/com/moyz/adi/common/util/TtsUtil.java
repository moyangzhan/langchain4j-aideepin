package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.vo.TtsSetting;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
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
import java.util.function.Consumer;

@Slf4j
public class TtsUtil {

    private static final int STREAM_BUFFER_SIZE = 2048;
    private static final int MIN_CALLBACK_BUFFER_SIZE = 16 * 1024;

    public static boolean needTts(TtsSetting ttsSetting, int answerContentType) {
        return AdiConstant.TtsConstant.SYNTHESIZER_SERVER.equals(ttsSetting.getSynthesizerSide()) && answerContentType == AdiConstant.CharacterConstant.ANSWER_CONTENT_TYPE_AUDIO;
    }

    /**
     * Execute TTS HTTP request, stream the audio response, buffer and callback chunks, then save to file.
     *
     * @param url             request URL
     * @param headers         HTTP headers
     * @param requestBody     JSON request body bytes
     * @param modelName       model name used for audio file naming (should be pre-sanitized)
     * @param processCallback callback to send audio chunks
     * @param completeCallback callback with saved file path on completion
     */
    public static void executeAndSave(String url, HttpHeaders headers, byte[] requestBody,
                                      String modelName, Consumer<ByteBuffer> processCallback,
                                      Consumer<String> completeCallback) {
        RestTemplate restTemplate = new RestTemplate();
        ResponseExtractor<Void> responseExtractor = response -> {
            String datetime = LocalDateTimeUtil.format(LocalDateTime.now(), LocalDateTimeUtil.PATTERN_YYYYMMDDMMHHSS);
            String fileName = modelName + "-" + datetime + "-" + UuidUtil.createShort().substring(0, 6) + ".mp3";
            Path tempFile = Files.createTempFile("tmp-tts-" + datetime, ".mp3");
            try {
                try (InputStream inputStream = response.getBody();
                     OutputStream outputStream = Files.newOutputStream(tempFile);
                     BufferedOutputStream bufferedOutput = new BufferedOutputStream(outputStream)) {
                    ByteArrayOutputStream sendBuffer = new ByteArrayOutputStream();
                    byte[] buffer = new byte[STREAM_BUFFER_SIZE];
                    int bytesRead;
                    while ((bytesRead = inputStream.read(buffer)) != -1) {
                        bufferedOutput.write(buffer, 0, bytesRead);
                        sendBuffer.write(buffer, 0, bytesRead);
                        if (sendBuffer.size() >= MIN_CALLBACK_BUFFER_SIZE) {
                            processCallback.accept(ByteBuffer.wrap(sendBuffer.toByteArray()));
                            sendBuffer.reset();
                        }
                    }
                    if (sendBuffer.size() > 0) {
                        processCallback.accept(ByteBuffer.wrap(sendBuffer.toByteArray()));
                    }
                }
                Pair<String, String> pair = new FileOperatorContext().save(Files.readAllBytes(tempFile), false, fileName);
                log.info("File saved successfully, path: {}", pair.getLeft());
                completeCallback.accept(pair.getLeft());
            } finally {
                Files.deleteIfExists(tempFile);
            }
            return null;
        };
        restTemplate.execute(url, HttpMethod.POST, requestCallback -> {
            requestCallback.getHeaders().putAll(headers);
            requestCallback.getBody().write(requestBody);
        }, responseExtractor);
    }
}
