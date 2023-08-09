package com.moyz.adi.common.helper;

import com.didalgo.gpt3.ChatFormatDescriptor;
import com.didalgo.gpt3.Encoding;
import com.didalgo.gpt3.GPT3Tokenizer;
import com.didalgo.gpt3.TokenCount;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiImage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.model.AnswerMeta;
import com.moyz.adi.common.model.ChatMeta;
import com.moyz.adi.common.model.QuestionMeta;
import com.moyz.adi.common.service.FileService;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.util.FileUtil;
import com.moyz.adi.common.util.ImageUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.TriConsumer;
import com.theokanning.openai.OpenAiApi;
import com.theokanning.openai.completion.chat.ChatCompletionChoice;
import com.theokanning.openai.completion.chat.ChatCompletionRequest;
import com.theokanning.openai.completion.chat.ChatMessage;
import com.theokanning.openai.image.*;
import com.theokanning.openai.service.OpenAiService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import retrofit2.Retrofit;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static com.moyz.adi.common.cosntant.AdiConstant.OPENAI_CREATE_IMAGE_RESP_FORMATS_URL;
import static com.moyz.adi.common.cosntant.AdiConstant.OPENAI_CREATE_IMAGE_SIZES;
import static com.theokanning.openai.service.OpenAiService.defaultClient;
import static com.theokanning.openai.service.OpenAiService.defaultRetrofit;

@Slf4j
@Service
public class OpenAiHelper {

    @Value("${openai.proxy.enable:false}")
    private boolean proxyEnable;

    @Value("${openai.proxy.host:0}")
    private String proxyHost;

    @Value("${openai.proxy.http-port:0}")
    private int proxyHttpPort;

    @Value("${local.images}")
    private String localImagesPath;

    @Resource
    private FileService fileService;

    @Resource
    private ObjectMapper objectMapper;

    public OpenAiService getOpenAiService(User user) {
        String secretKey = SysConfigService.getSecretKey();
        String userSecretKey = user.getSecretKey();
        if (StringUtils.isNotBlank(userSecretKey)) {
            secretKey = userSecretKey;
        }
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

    /**
     * Send http request to openai server <br/>
     * Calculate token
     *
     * @param user
     * @param regenerateQuestionUuid
     * @param chatMessageList
     * @param sseEmitter
     * @param consumer
     */
    public void sseAsk(User user, String regenerateQuestionUuid, List<ChatMessage> chatMessageList, SseEmitter sseEmitter, TriConsumer<String, QuestionMeta, AnswerMeta> consumer) {
        final int[] answerTokens = {0};
        StringBuilder response = new StringBuilder();
        OpenAiService service = getOpenAiService(user);
        ChatCompletionRequest chatCompletionRequest = ChatCompletionRequest
                .builder()
                .model(AdiConstant.DEFAULT_MODEL)
                .messages(chatMessageList)
                .n(1)
                .logitBias(new HashMap<>())
                .build();
        service.streamChatCompletion(chatCompletionRequest)
                .doOnError(onError -> {
                    log.error("openai error", onError);
                    sseEmitter.send(SseEmitter.event().name("error").data(onError.getMessage()));
                    sseEmitter.complete();
                }).subscribe(completionChunk -> {
                    answerTokens[0]++;
                    List<ChatCompletionChoice> choices = completionChunk.getChoices();
                    String content = choices.get(0).getMessage().getContent();
                    log.info("get content:{}", content);
                    if (null == content && response.isEmpty()) {
                        return;
                    }
                    if (null == content || AdiConstant.OPENAI_MESSAGE_DONE_FLAG.equals(content)) {
                        log.info("OpenAI返回数据结束了");
                        sseEmitter.send(AdiConstant.OPENAI_MESSAGE_DONE_FLAG);


                        GPT3Tokenizer tokenizer = new GPT3Tokenizer(Encoding.CL100K_BASE);
                        int questionTokens = 0;
                        try {
                            questionTokens = TokenCount.fromMessages(chatMessageList, tokenizer, ChatFormatDescriptor.forModel(AdiConstant.DEFAULT_MODEL));
                        } catch (IllegalArgumentException e) {
                            log.error("该模型的token无法统计,model:{}", AdiConstant.DEFAULT_MODEL);
                        }
                        System.out.println("requestTokens:" + questionTokens);
                        System.out.println("返回内容：" + response);

                        String questionUuid = StringUtils.isNotBlank(regenerateQuestionUuid) ? regenerateQuestionUuid : UUID.randomUUID().toString().replace("-", "");
                        QuestionMeta questionMeta = new QuestionMeta(questionTokens, questionUuid);
                        AnswerMeta answerMeta = new AnswerMeta(answerTokens[0], UUID.randomUUID().toString().replace("-", ""));
                        ChatMeta chatMeta = new ChatMeta(questionMeta, answerMeta);
//                        String meta = JsonUtil.toJson(chatMeta).replaceAll("\r\n", "");
                        String meta = JsonUtil.toJson(chatMeta).replaceAll("\r\n", "");
                        log.info("meta:" + meta);
                        sseEmitter.send(" [META]" + meta);
                        // close eventSourceEmitter after tokens was calculated
                        sseEmitter.complete();
                        consumer.accept(response.toString(), questionMeta, answerMeta);
                        return;
                    }
                    //加空格配合前端的fetchEventSource进行解析，见https://github.com/Azure/fetch-event-source/blob/45ac3cfffd30b05b79fbf95c21e67d4ef59aa56a/src/parse.ts#L129-L133
                    sseEmitter.send(" " + content);
                    response.append(content);
                });


        System.out.println("返回内容1111：" + response);
    }

    public List<Image> createImage(User user, AiImage aiImage) {
        if (aiImage.getGenerateNumber() < 1 || aiImage.getGenerateNumber() > 10) {
            throw new BaseException(ErrorEnum.A_IMAGE_NUMBER_ERROR);
        }
        if (!OPENAI_CREATE_IMAGE_SIZES.contains(aiImage.getGenerateSize())) {
            throw new BaseException(ErrorEnum.A_IMAGE_SIZE_ERROR);
        }
        OpenAiService service = getOpenAiService(user);
        CreateImageRequest createImageRequest = new CreateImageRequest();
        createImageRequest.setPrompt(aiImage.getPrompt());
        createImageRequest.setN(aiImage.getGenerateNumber());
        createImageRequest.setSize(aiImage.getGenerateSize());
        createImageRequest.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        createImageRequest.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImage(createImageRequest);
            log.info("createImage response:{}", imageResult);
            return imageResult.getData();
        } catch (Exception e) {
            log.error("create image error", e);
        }
        return Collections.emptyList();
    }

    public List<Image> editImage(User user, AiImage aiImage) {
        File originalFile = new File(fileService.getImagePath(aiImage.getOriginalImage()));
        File maskFile = null;
        if (StringUtils.isNotBlank(aiImage.getMaskImage())) {
            maskFile = new File(fileService.getImagePath(aiImage.getMaskImage()));
        }
        //如果不是RGBA类型的图片，先转成RGBA
        File rgbaOriginalImage = ImageUtil.rgbConvertToRgba(originalFile, fileService.getTmpImagesPath(aiImage.getOriginalImage()));
        OpenAiService service = getOpenAiService(user);
        CreateImageEditRequest request = new CreateImageEditRequest();
        request.setPrompt(aiImage.getPrompt());
        request.setN(aiImage.getGenerateNumber());
        request.setSize(aiImage.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageEdit(request, rgbaOriginalImage, maskFile);
            log.info("editImage response:{}", imageResult);
            return imageResult.getData();
        } catch (Exception e) {
            log.error("edit image error", e);
        }
        return Collections.emptyList();
    }

    public List<Image> createImageVariation(User user, AiImage aiImage) {
        File imagePath = new File(fileService.getImagePath(aiImage.getOriginalImage()));
        OpenAiService service = getOpenAiService(user);
        CreateImageVariationRequest request = new CreateImageVariationRequest();
        request.setN(aiImage.getGenerateNumber());
        request.setSize(aiImage.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageVariation(request, imagePath);
            log.info("createImageVariation response:{}", imageResult);
            return imageResult.getData();
        } catch (Exception e) {
            log.error("image variation error", e);
        }
        return Collections.emptyList();
    }

}
