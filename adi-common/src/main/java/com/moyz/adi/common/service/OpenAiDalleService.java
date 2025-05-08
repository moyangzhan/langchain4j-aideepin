package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.interfaces.AbstractImageModelService;
import com.moyz.adi.common.util.ImageUtil;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.OpenAiUtil;
import com.moyz.adi.common.vo.LLMException;
import com.moyz.adi.common.vo.OpenAiSetting;
import com.theokanning.openai.OpenAiApi;
import com.theokanning.openai.image.CreateImageEditRequest;
import com.theokanning.openai.image.CreateImageVariationRequest;
import com.theokanning.openai.image.Image;
import com.theokanning.openai.image.ImageResult;
import com.theokanning.openai.service.OpenAiService;
import dev.langchain4j.model.image.ImageModel;
import dev.langchain4j.model.openai.OpenAiImageModel;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import org.apache.commons.lang3.StringUtils;
import retrofit2.Retrofit;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.*;
import static com.theokanning.openai.service.OpenAiService.defaultClient;
import static com.theokanning.openai.service.OpenAiService.defaultRetrofit;
import static dev.ai4j.openai4j.image.ImageModel.DALL_E_SIZE_512_x_512;
import static dev.langchain4j.model.openai.OpenAiImageModelName.DALL_E_2;
import static dev.langchain4j.model.openai.OpenAiImageModelName.DALL_E_3;

@Slf4j
public class OpenAiDalleService extends AbstractImageModelService<OpenAiSetting> {

    public OpenAiDalleService(AiModel aiModel) {
        super(aiModel, AdiConstant.SysConfigKey.OPENAI_SETTING, OpenAiSetting.class);
    }

    @Override
    public boolean isEnabled() {
        return StringUtils.isNotBlank(setting.getSecretKey()) && aiModel.getIsEnable();
    }

    @Override
    public List<String> generateImage(User user, Draw draw) {
        if (draw.getAiModelName().equalsIgnoreCase(DALL_E_2.toString())) {
            assertDalle2(draw);
        } else {
            assertDalle3(draw);
        }
        return super.generateImage(user, draw);
    }

    private void assertDalle2(Draw draw) {
        if (!DALLE2_CREATE_IMAGE_SIZES.contains(draw.getGenerateSize())) {
            throw new BaseException(ErrorEnum.A_IMAGE_SIZE_ERROR);
        }
        if (draw.getGenerateNumber() < 1 || draw.getGenerateNumber() > 10) {
            throw new BaseException(ErrorEnum.A_IMAGE_NUMBER_ERROR);
        }
    }

    private void assertDalle3(Draw draw) {
        if (!DALLE3_CREATE_IMAGE_SIZES.contains(draw.getGenerateSize())) {
            throw new BaseException(ErrorEnum.A_IMAGE_SIZE_ERROR);
        }
    }

    /**
     * You can request 1 image at a time with DALL·E 3 (request more by making parallel requests) or up to 10 images at a time using DALL·E 2 with the n parameter.
     *
     * @param user
     * @param draw
     * @return
     */
    @Override
    public ImageModel buildImageModel(User user, Draw draw) {
        if (StringUtils.isBlank(setting.getSecretKey())) {
            throw new BaseException(ErrorEnum.B_LLM_SECRET_KEY_NOT_SET);
        }
        if (aiModel.getName().equals(DALL_E_3.toString())) {
            return createDalle3Model(user, draw);
        } else {
            return createDalle2Model(user, draw);
        }
    }

    /**
     * dall-e-2
     *
     * @param user
     * @param draw
     * @return
     */
    private ImageModel createDalle2Model(User user, Draw draw) {
        OpenAiImageModel.OpenAiImageModelBuilder builder = OpenAiImageModel.builder()
                .baseUrl(setting.getBaseUrl())
                .modelName(aiModel.getName())
                .apiKey(setting.getSecretKey())
                .user(user.getUuid())
                .responseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL)
                .size(StringUtils.defaultString(draw.getGenerateSize(), DALL_E_SIZE_512_x_512))
                .logRequests(true)
                .logResponses(true)
                .withPersisting(false)
                .maxRetries(1);
        if (null != proxy) {
            builder.proxy(proxy);
        }
        return builder.build();
    }

    /**
     * dall-e-3
     *
     * @param user
     * @param draw
     * @return
     */

    private ImageModel createDalle3Model(User user, Draw draw) {
        OpenAiImageModel.OpenAiImageModelBuilder builder = OpenAiImageModel.builder()
                .baseUrl(setting.getBaseUrl())
                .modelName(DALL_E_3.toString())
                .apiKey(setting.getSecretKey())
                .user(user.getUuid())
                .responseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL)
                .size(draw.getGenerateSize())
                .quality(draw.getGenerateQuality())
                .logRequests(true)
                .logResponses(true)
                .withPersisting(false)
                .maxRetries(1);
        if (null != proxy) {
            builder.proxy(proxy);
        }
        return builder.build();
    }

    @Override
    public List<String> editImage(User user, Draw draw) {
        File originalFile = new File(getFileService().getImagePath(draw.getOriginalImage()));
        File maskFile = null;
        if (StringUtils.isNotBlank(draw.getMaskImage())) {
            maskFile = new File(getFileService().getImagePath(draw.getMaskImage()));
        }
        //如果不是RGBA类型的图片，先转成RGBA
        File rgbaOriginalImage = ImageUtil.rgbConvertToRgba(originalFile, getFileService().getTmpImagesPath(draw.getOriginalImage()));
        OpenAiService service = getOpenAiService();
        CreateImageEditRequest request = new CreateImageEditRequest();
        request.setPrompt(draw.getPrompt());
        request.setN(draw.getGenerateNumber());
        request.setSize(draw.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageEdit(request, rgbaOriginalImage, maskFile);
            log.info("editImage response:{}", imageResult);
            return imageResult.getData().stream().map(Image::getUrl).toList();
        } catch (Exception e) {
            log.error("edit image error", e);
        }
        return Collections.emptyList();
    }

    @Override
    public List<String> createImageVariation(User user, Draw draw) {
        File imagePath = new File(getFileService().getImagePath(draw.getOriginalImage()));
        OpenAiService service = getOpenAiService();
        CreateImageVariationRequest request = new CreateImageVariationRequest();
        request.setN(draw.getGenerateNumber());
        request.setSize(draw.getGenerateSize());
        request.setResponseFormat(OPENAI_CREATE_IMAGE_RESP_FORMATS_URL);
        request.setUser(user.getUuid());
        try {
            ImageResult imageResult = service.createImageVariation(request, imagePath);
            log.info("createImageVariation response:{}", imageResult);
            return imageResult.getData().stream().map(Image::getUrl).toList();
        } catch (Exception e) {
            log.error("image variation error", e);
        }
        return Collections.emptyList();
    }

    public OpenAiService getOpenAiService() {
        String secretKey = setting.getSecretKey();
        if (null != proxy) {
            OkHttpClient client = defaultClient(secretKey, Duration.of(60, ChronoUnit.SECONDS))
                    .newBuilder()
                    .proxy(proxy)
                    .build();
            Retrofit retrofit = defaultRetrofit(client, JsonUtil.getObjectMapper());
            OpenAiApi api = retrofit.create(OpenAiApi.class);
            return new OpenAiService(api);
        }
        return new OpenAiService(secretKey, Duration.of(60, ChronoUnit.SECONDS));
    }

    @Override
    protected LLMException parseError(Object error) {
        return OpenAiUtil.parseError(error);
    }
}
