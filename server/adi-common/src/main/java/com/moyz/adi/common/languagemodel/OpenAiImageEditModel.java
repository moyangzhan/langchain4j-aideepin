package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.languagemodel.data.OpenAiImageEditResp;
import com.moyz.adi.common.util.JsonUtil;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.output.Response;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ProxySelector;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * OpenAI gpt-image 系图生图封装:调用 /images/edits 端点(multipart)。
 * <p>
 * langchain4j 内置 OpenAiImageModel 仅覆盖文生图(images/generations),图像编辑在此自研实现;
 * 参考图字节由服务端读取后直传,不依赖文件公网可访问。
 */
@Slf4j
@Builder
public class OpenAiImageEditModel implements ImageModel {

    public static final String DEFAULT_BASE_URL = "https://api.openai.com/v1";

    private final String baseUrl;
    private final String apiKey;
    private final String modelName;
    /**
     * 尺寸;空或 "auto" 表示交给服务端默认
     */
    private final String size;
    /**
     * 质量;空或 "auto" 表示交给服务端默认
     */
    private final String quality;
    private final List<RefImage> refImages;
    private final InetSocketAddress proxyAddress;

    /**
     * 参考图:文件字节 + MIME 类型 + multipart 文件名
     * <p>
     * Reference image: file bytes + MIME type + multipart filename.
     */
    public record RefImage(byte[] bytes, String mimeType, String filename) {
    }

    @Override
    public Response<Image> generate(String prompt) {
        return Response.from(editImages(prompt, 1).get(0));
    }

    @Override
    public Response<List<Image>> generate(String prompt, int n) {
        return Response.from(editImages(prompt, n));
    }

    private List<Image> editImages(String prompt, int n) {
        String boundary = "----AdiBoundary" + UUID.randomUUID().toString().replace("-", "");
        List<byte[]> parts = new ArrayList<>();
        addTextPart(parts, boundary, "model", modelName);
        addTextPart(parts, boundary, "prompt", prompt);
        addTextPart(parts, boundary, "n", String.valueOf(n));
        if (StringUtils.isNotBlank(size) && !"auto".equalsIgnoreCase(size)) {
            addTextPart(parts, boundary, "size", size);
        }
        if (StringUtils.isNotBlank(quality) && !"auto".equalsIgnoreCase(quality)) {
            addTextPart(parts, boundary, "quality", quality);
        }
        for (RefImage refImage : refImages) {
            parts.add(filePart(boundary, "image[]", refImage));
        }
        parts.add(("--" + boundary + "--\r\n").getBytes(StandardCharsets.UTF_8));

        HttpRequest request = HttpRequest.newBuilder(URI.create(editsUrl()))
                .header("Authorization", "Bearer " + apiKey)
                .header("Content-Type", "multipart/form-data; boundary=" + boundary)
                .POST(HttpRequest.BodyPublishers.ofByteArray(concat(parts)))
                .build();
        String responseBody;
        try {
            HttpClient.Builder clientBuilder = HttpClient.newBuilder();
            if (null != proxyAddress) {
                clientBuilder.proxy(ProxySelector.of(proxyAddress));
            }
            HttpResponse<String> response = clientBuilder.build().send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                log.error("OpenAI image edit fail, status:{}, body:{}", response.statusCode(), response.body());
                throw new BaseException(ErrorEnum.C_DRAW_FAIL, response.body());
            }
            responseBody = response.body();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, "OpenAI image edit interrupted");
        } catch (IOException e) {
            log.error("OpenAI image edit error", e);
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, e.getMessage());
        }

        OpenAiImageEditResp resp = JsonUtil.fromJson(responseBody, OpenAiImageEditResp.class);
        if (null == resp || null == resp.getData()) {
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, "OpenAI image edit response is null");
        }
        List<Image> result = new ArrayList<>();
        for (OpenAiImageEditResp.Item item : resp.getData()) {
            if (StringUtils.isNotBlank(item.getB64Json())) {
                result.add(Image.builder().base64Data(item.getB64Json()).mimeType("image/png").build());
            } else if (StringUtils.isNotBlank(item.getUrl())) {
                result.add(Image.builder().url(item.getUrl()).build());
            }
        }
        if (result.isEmpty()) {
            throw new BaseException(ErrorEnum.C_DRAW_FAIL, "OpenAI image edit returned no image");
        }
        return result;
    }

    private String editsUrl() {
        String base = StringUtils.isBlank(baseUrl) ? DEFAULT_BASE_URL : baseUrl;
        return base.endsWith("/") ? base + "images/edits" : base + "/images/edits";
    }

    private static void addTextPart(List<byte[]> parts, String boundary, String name, String value) {
        parts.add(("--" + boundary + "\r\n"
                + "Content-Disposition: form-data; name=\"" + name + "\"\r\n\r\n")
                .getBytes(StandardCharsets.UTF_8));
        parts.add(value.getBytes(StandardCharsets.UTF_8));
        parts.add("\r\n".getBytes(StandardCharsets.UTF_8));
    }

    private static byte[] filePart(String boundary, String name, RefImage refImage) {
        String header = "--" + boundary + "\r\n"
                + "Content-Disposition: form-data; name=\"" + name + "\"; filename=\"" + refImage.filename() + "\"\r\n"
                + "Content-Type: " + refImage.mimeType() + "\r\n\r\n";
        byte[] headerBytes = header.getBytes(StandardCharsets.UTF_8);
        byte[] crlf = "\r\n".getBytes(StandardCharsets.UTF_8);
        byte[] part = new byte[headerBytes.length + refImage.bytes().length + crlf.length];
        System.arraycopy(headerBytes, 0, part, 0, headerBytes.length);
        System.arraycopy(refImage.bytes(), 0, part, headerBytes.length, refImage.bytes().length);
        System.arraycopy(crlf, 0, part, headerBytes.length + refImage.bytes().length, crlf.length);
        return part;
    }

    private static byte[] concat(List<byte[]> parts) {
        int total = 0;
        for (byte[] part : parts) {
            total += part.length;
        }
        byte[] out = new byte[total];
        int pos = 0;
        for (byte[] part : parts) {
            System.arraycopy(part, 0, out, pos, part.length);
            pos += part.length;
        }
        return out;
    }
}
