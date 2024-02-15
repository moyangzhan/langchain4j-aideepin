package com.moyz.adi.common.helper;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.AiImage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.IChatAssistant;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.util.ImageUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.TriConsumer;
import com.moyz.adi.common.vo.AnswerMeta;
import com.moyz.adi.common.vo.ChatMeta;
import com.moyz.adi.common.vo.QuestionMeta;
import com.moyz.adi.common.vo.SseAskParams;
import com.theokanning.openai.OpenAiApi;
import com.theokanning.openai.image.CreateImageEditRequest;
import com.theokanning.openai.image.CreateImageVariationRequest;
import com.theokanning.openai.image.ImageResult;
import com.theokanning.openai.service.OpenAiService;
import dev.langchain4j.data.image.Image;
import dev.langchain4j.memory.ChatMemory;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.openai.OpenAiImageModel;
import dev.langchain4j.model.openai.OpenAiStreamingChatModel;
import dev.langchain4j.model.output.Response;
import dev.langchain4j.service.AiServices;
import dev.langchain4j.service.TokenStream;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import retrofit2.Retrofit;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.OPENAI_CREATE_IMAGE_RESP_FORMATS_URL;
import static com.moyz.adi.common.cosntant.AdiConstant.OPENAI_CREATE_IMAGE_SIZES;
import static com.theokanning.openai.service.OpenAiService.defaultClient;
import static com.theokanning.openai.service.OpenAiService.defaultRetrofit;
import static dev.ai4j.openai4j.image.ImageModel.DALL_E_SIZE_1024_x_1024;
import static dev.ai4j.openai4j.image.ImageModel.DALL_E_SIZE_512_x_512;
import static dev.langchain4j.model.openai.OpenAiModelName.DALL_E_2;

@Slf4j
@Service
public class OpenAiHelper {

    @Value("${openai.proxy.enable:false}")
    private boolean proxyEnable;

    @Value("${openai.proxy.host:0}")
    private String proxyHost;

    @Value("${openai.proxy.http-port:0}")
    private int proxyHttpPort;

    @Resource
    private FileService fileService;

    @Resource
    private ObjectMapper objectMapper;

    public String getSecretKey() {
        String secretKey = SysConfigService.getSecretKey();
        User user = ThreadContext.getCurrentUser();
        if (null != user && StringUtils.isNotBlank(user.getSecretKey())) {
            secretKey = user.getSecretKey();
        }
        return secretKey;
    }

    public OpenAiService getOpenAiService() {
        String secretKey = getSecretKey();
        if (proxyEnable) {
            Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
            OkHttpClient client = defaultClient(secretKey, Duration.of(60, ChronoUnit.SECONDS))
                    .newBuilder()
                    .proxy(proxy)
                    .build();
            Retrofit retrofit = defaultRetrofit(client, objectMapper);
            OpenAiApi api = retrofit.create(OpenAiApi.class);
            return new OpenAiService(api);
        }
        return new OpenAiService(secretKey, Duration.of(60, ChronoUnit.SECONDS));
    }

    public IChatAssistant getChatAssistant(ChatMemory chatMemory) {
        String secretKey = getSecretKey();
        OpenAiStreamingChatModel.OpenAiStreamingChatModelBuilder builder = OpenAiStreamingChatModel.builder();
        if (proxyEnable) {
            Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
            builder.proxy(proxy);
        }
        builder.apiKey(secretKey).timeout(Duration.of(60, ChronoUnit.SECONDS));
        AiServices<IChatAssistant> serviceBuilder = AiServices.builder(IChatAssistant.class)
                .streamingChatLanguageModel(builder.build());
        if (null != chatMemory) {
            serviceBuilder.chatMemory(chatMemory);
        }
        return serviceBuilder.build();
    }

    public ImageModel getImageModel(User user, String size) {
        String secretKey = getSecretKey();
        if (proxyEnable) {
            Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, proxyHttpPort));
            return OpenAiImageModel.builder()
                    .modelName(DALL_E_2)
                    .apiKey(secretKey)
                    .user(user.getUuid())
                    .responseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL)
                    .size(StringUtils.defaultString(size, DALL_E_SIZE_512_x_512))
                    .logRequests(true)
                    .logResponses(true)
                    .withPersisting(false)
                    .maxRetries(2)
                    .proxy(proxy)
                    .build();
        }
        return OpenAiImageModel.builder()
                .modelName(DALL_E_2)
                .apiKey(secretKey)
                .user(user.getUuid())
                .responseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL)
                .size(StringUtils.defaultString(size, DALL_E_SIZE_512_x_512))
                .logRequests(true)
                .logResponses(true)
                .withPersisting(false)
                .maxRetries(2)
                .build();
    }

    /**
     * Send http request to llm server
     */
    public void sseAsk(SseAskParams params, TriConsumer<String, QuestionMeta, AnswerMeta> consumer) {
        IChatAssistant chatAssistant = getChatAssistant(params.getChatMemory());
        TokenStream tokenStream;
        if (StringUtils.isNotBlank(params.getSystemMessage())) {
            tokenStream = chatAssistant.chat(params.getSystemMessage(), params.getUserMessage());
        } else {
            tokenStream = chatAssistant.chat(params.getUserMessage());
        }
        tokenStream.onNext((content) -> {
                    log.info("get content:{}", content);
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    try {
                        params.getSseEmitter().send(" " + content);
                    } catch (IOException e) {
                        log.error("stream onNext error", e);
                    }
                })
                .onComplete((response) -> {
                    log.info("返回数据结束了:{}", response);
                    String questionUuid = StringUtils.isNotBlank(params.getRegenerateQuestionUuid()) ? params.getRegenerateQuestionUuid() : UUID.randomUUID().toString().replace("-", "");
                    QuestionMeta questionMeta = new QuestionMeta(response.tokenUsage().inputTokenCount(), questionUuid);
                    AnswerMeta answerMeta = new AnswerMeta(response.tokenUsage().outputTokenCount(), UUID.randomUUID().toString().replace("-", ""));
                    ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
                    String meta = JsonUtil.toJson(chatMeta).replaceAll("\r\n", "");
                    log.info("meta:" + meta);
                    try {
                        params.getSseEmitter().send(" [META]" + meta);
                    } catch (IOException e) {
                        log.error("stream onComplete error", e);
                        throw new RuntimeException(e);
                    }
                    // close eventSourceEmitter after tokens was calculated
                    params.getSseEmitter().complete();
                    consumer.accept(response.content().text(), questionMeta, answerMeta);
                })
                .onError((error) -> {
                    log.error("stream error", error);
                    try {
                        params.getSseEmitter().send(SseEmitter.event().name("error").data(error.getMessage()));
                    } catch (IOException e) {
                        log.error("sse error", e);
                    }
                    params.getSseEmitter().complete();
                })
                .start();
    }

    public List<String> createImage(User user, AiImage aiImage) {
        if (aiImage.getGenerateNumber() < 1 || aiImage.getGenerateNumber() > 10) {
            throw new BaseException(ErrorEnum.A_IMAGE_NUMBER_ERROR);
        }
        if (!OPENAI_CREATE_IMAGE_SIZES.contains(aiImage.getGenerateSize())) {
            throw new BaseException(ErrorEnum.A_IMAGE_SIZE_ERROR);
        }
        ImageModel imageModel = getImageModel(user, aiImage.getGenerateSize());
        try {
            Response<List<Image>> response = imageModel.generate(aiImage.getPrompt(), aiImage.getGenerateNumber());
            log.info("createImage response:{}", response);
            return response.content().stream().map(item -> item.url().toString()).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("create image error", e);
        }
        return Collections.emptyList();
    }

    public List<String> editImage(User user, AiImage aiImage) {
        File originalFile = new File(fileService.getImagePath(aiImage.getOriginalImage()));
        File maskFile = null;
        if (StringUtils.isNotBlank(aiImage.getMaskImage())) {
            maskFile = new File(fileService.getImagePath(aiImage.getMaskImage()));
        }
        //如果不是RGBA类型的图片，先转成RGBA
        File rgbaOriginalImage = ImageUtil.rgbConvertToRgba(originalFile, fileService.getTmpImagesPath(aiImage.getOriginalImage()));
        OpenAiService service = getOpenAiService();
        CreateImageEditRequest request = new CreateImageEditRequest();
        request.setPrompt(aiImage.getPrompt());
        request.setN(aiImage.getGenerateNumber());
        request.setSize(aiImage.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageEdit(request, rgbaOriginalImage, maskFile);
            log.info("editImage response:{}", imageResult);
            return imageResult.getData().stream().map(item -> item.getUrl()).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("edit image error", e);
        }
        return Collections.emptyList();
    }

    public List<String> createImageVariation(User user, AiImage aiImage) {
        File imagePath = new File(fileService.getImagePath(aiImage.getOriginalImage()));
        OpenAiService service = getOpenAiService();
        CreateImageVariationRequest request = new CreateImageVariationRequest();
        request.setN(aiImage.getGenerateNumber());
        request.setSize(aiImage.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageVariation(request, imagePath);
            log.info("createImageVariation response:{}", imageResult);
            return imageResult.getData().stream().map(item -> item.getUrl()).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("image variation error", e);
        }
        return Collections.emptyList();
    }

}
