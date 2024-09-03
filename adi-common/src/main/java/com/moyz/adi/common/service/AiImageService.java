package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.AiImage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.UserDayCost;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.RateLimitHelper;
import com.moyz.adi.common.mapper.AiImageMapper;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UserUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestParam;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.GenerateImage.*;
import static com.moyz.adi.common.cosntant.AdiConstant.IMAGE_PATH_PRE;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class AiImageService extends ServiceImpl<AiImageMapper, AiImage> {

    @Resource
    @Lazy
    private AiImageService _this;
    @Resource
    private QuotaHelper quotaHelper;

    @Resource
    private RateLimitHelper rateLimitHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private FileService fileService;

    public void check() {
        User user = ThreadContext.getCurrentUser();
        String askingKey = MessageFormat.format(RedisKeyConstant.USER_DRAWING, user.getId());
        String askingVal = stringRedisTemplate.opsForValue().get(askingKey);
        //check 1: still waiting response
        if (StringUtils.isNotBlank(askingVal)) {
            throw new BaseException(A_DRAWING);
        }

        String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
        if (!rateLimitHelper.checkRequestTimes(requestTimesKey, LocalCache.TEXT_RATE_LIMIT_CONFIG)) {
            throw new BaseException(A_REQUEST_TOO_MUCH);
        }
        ErrorEnum errorEnum = quotaHelper.checkImageQuota(user);
        if (null != errorEnum) {
            throw new BaseException(errorEnum);
        }
    }

    /**
     * interacting method 1: Creates an image given a prompt
     *
     * @param generateImageReq
     */
    public String createByPrompt(GenerateImageReq generateImageReq) {
        _this.check();
        CreateImageDto createImageDto = new CreateImageDto();
        createImageDto.setInteractingMethod(INTERACTING_METHOD_GENERATE_IMAGE);
        BeanUtils.copyProperties(generateImageReq, createImageDto);
        return _this.generate(createImageDto);
    }

    /**
     * Interacting method 2:  Creates an edited or extended image given an original image and a prompt.
     */
    public String editByOriginalImage(EditImageReq editImageReq) {
        _this.check();
        CreateImageDto createImageDto = new CreateImageDto();
        createImageDto.setInteractingMethod(INTERACTING_METHOD_EDIT_IMAGE);
        createImageDto.setPrompt(editImageReq.getPrompt());
        createImageDto.setSize(editImageReq.getSize());
        createImageDto.setNumber(editImageReq.getNumber());
        createImageDto.setMaskImage(editImageReq.getMaskImage());
        createImageDto.setOriginalImage(editImageReq.getOriginalImage());
        return _this.generate(createImageDto);
    }

    /**
     * interacting method 3: Creates a variation of a given image.
     */
    public String variationImage(VariationImageReq variationImageReq) {
        _this.check();
        CreateImageDto createImageDto = new CreateImageDto();
        createImageDto.setInteractingMethod(INTERACTING_METHOD_VARIATION);
        createImageDto.setSize(variationImageReq.getSize());
        createImageDto.setNumber(variationImageReq.getNumber());
        createImageDto.setOriginalImage(variationImageReq.getOriginalImage());
        return _this.generate(createImageDto);
    }

    /**
     * 根据提示词生成图片
     *
     * @param createImageDto
     * @return
     */
    public String generate(CreateImageDto createImageDto) {
        User user = ThreadContext.getCurrentUser();
        int generateNumber = Math.min(createImageDto.getNumber(), user.getQuotaByImageDaily());
        String uuid = UuidUtil.createShort();
        AiImage aiImage = new AiImage();
        aiImage.setGenerateSize(createImageDto.getSize());
        aiImage.setGenerateQuality(createImageDto.getQuality());
        aiImage.setGenerateNumber(generateNumber);
        aiImage.setUuid(uuid);
        aiImage.setAiModelName(createImageDto.getModelName());
        aiImage.setUserId(user.getId());
        aiImage.setInteractingMethod(createImageDto.getInteractingMethod());
        aiImage.setProcessStatus(STATUS_DOING);
        aiImage.setPrompt(createImageDto.getPrompt());
        aiImage.setOriginalImage(createImageDto.getOriginalImage());
        aiImage.setMaskImage(createImageDto.getMaskImage());
        getBaseMapper().insert(aiImage);
        AiImage obj = this.lambdaQuery().eq(AiImage::getUuid, uuid).one();
        _this.createFromRemote(obj, user);
        return uuid;
    }

    /**
     * Regenerate the image that was fail
     *
     * @param uuid
     */
    public void regenerate(String uuid) {
        User user = ThreadContext.getCurrentUser();
        AiImage obj = this.lambdaQuery()
                .eq(AiImage::getUuid, uuid)
                .eq(AiImage::getProcessStatus, STATUS_FAIL)
                .oneOpt().orElseThrow(() -> new BaseException(B_FIND_IMAGE_404));

        _this.createFromRemote(obj, user);
    }

    /**
     * 异步生成图片
     *
     * @param aiImage
     * @param user
     */
    @Async("imagesExecutor")
    public void createFromRemote(AiImage aiImage, User user) {
        String drawingKey = MessageFormat.format(RedisKeyConstant.USER_DRAWING, user.getId());
        stringRedisTemplate.opsForValue().set(drawingKey, "1", 30, TimeUnit.SECONDS);

        try {
            //Increase the number of the request
            String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
            rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.IMAGE_RATE_LIMIT_CONFIG);

            List<String> images = new ArrayList<>();
            if (aiImage.getInteractingMethod() == INTERACTING_METHOD_GENERATE_IMAGE) {
                images = ImageModelContext.getModelService(aiImage.getAiModelName()).generateImage(user, aiImage);
            } else if (aiImage.getInteractingMethod() == INTERACTING_METHOD_EDIT_IMAGE) {
                images = ImageModelContext.getModelService(aiImage.getAiModelName()).editImage(user, aiImage);
            } else if (aiImage.getInteractingMethod() == INTERACTING_METHOD_VARIATION) {
                images = ImageModelContext.getModelService(aiImage.getAiModelName()).createImageVariation(user, aiImage);
            }
            List<String> imageUuids = new ArrayList<>();
            images.forEach(imageUrl -> {
                String imageUuid = fileService.saveImageToLocal(user, imageUrl);
                imageUuids.add(imageUuid);
            });
            String imageUuidsJoin = imageUuids.stream().collect(Collectors.joining(","));
            if (StringUtils.isBlank(imageUuidsJoin)) {
                _this.lambdaUpdate().eq(AiImage::getId, aiImage.getId()).set(AiImage::getProcessStatus, STATUS_FAIL).update();
                return;
            }
            String respImagesPath = images.stream().collect(Collectors.joining(","));
            updateAiImageStatus(aiImage.getId(), respImagesPath, imageUuidsJoin, STATUS_SUCCESS);

            //Update the cost of current user
            UserDayCost userDayCost = userDayCostService.getTodayCost(user);
            UserDayCost saveOrUpdateInst = new UserDayCost();
            if (null == userDayCost) {
                saveOrUpdateInst.setUserId(user.getId());
                saveOrUpdateInst.setDay(LocalDateTimeUtil.getToday());
                saveOrUpdateInst.setImagesNumber(images.size());
                saveOrUpdateInst.setSecretKeyType(UserUtil.getSecretType(user));
            } else {
                saveOrUpdateInst.setId(userDayCost.getId());
                saveOrUpdateInst.setImagesNumber(userDayCost.getImagesNumber() + images.size());
            }
            userDayCostService.saveOrUpdate(saveOrUpdateInst);
        } finally {
            stringRedisTemplate.delete(drawingKey);
        }
    }

    public void updateAiImageStatus(Long aiImageId, String respImagesPath, String localImagesUuid, int generationStatus) {
        AiImage updateImage = new AiImage();
        updateImage.setId(aiImageId);
        updateImage.setRespImagesPath(respImagesPath);
        updateImage.setGeneratedImages(localImagesUuid);
        updateImage.setProcessStatus(generationStatus);
        getBaseMapper().updateById(updateImage);

        AdiFile adiFile = fileService.lambdaQuery().eq(AdiFile::getUuid, localImagesUuid).oneOpt().orElse(null);
        if (null != adiFile) {
            fileService.lambdaUpdate().eq(AdiFile::getId, adiFile.getId()).set(AdiFile::getRefCount, adiFile.getRefCount() + 1).update();
        }
    }


    public AiImagesListResp listAll(@RequestParam Long maxId, @RequestParam int pageSize) {
        List<AiImage> list = this.lambdaQuery()
                .eq(AiImage::getUserId, ThreadContext.getCurrentUserId())
                .eq(AiImage::getIsDeleted, false)
                .lt(AiImage::getId, maxId)
                .orderByDesc(AiImage::getId)
                .last("limit " + pageSize)
                .list();
        list.sort(Comparator.comparing(AiImage::getId));
        List<AiImageDto> dtoList = new ArrayList<>();
        list.forEach(item -> dtoList.add(convertAiImageToDto(item)));
        AiImagesListResp result = new AiImagesListResp();
        result.setImageItems(dtoList);
        result.setMinId(list.stream().map(AiImage::getId).reduce(Long.MAX_VALUE, Long::min));
        return result;
    }

    public AiImageDto getOne(String uuid) {
        AiImage aiImage = this.lambdaQuery()
                .eq(AiImage::getUuid, uuid)
                .eq(AiImage::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt()
                .orElse(null);
        if (null != aiImage) {
            return convertAiImageToDto(aiImage);
        } else {
            return null;
        }
    }

    /**
     * 删除做图记录
     *
     * @param uuid
     * @return
     */
    public boolean del(String uuid) {
        AiImage aiImage = checkAndGet(uuid);
        if (StringUtils.isNotBlank(aiImage.getGeneratedImages())) {
            String[] uuids = aiImage.getGeneratedImages().split(",");
            for (String fileUuid : uuids) {
                fileService.removeFileAndSoftDel(fileUuid);
            }
        }
        _this.softDel(aiImage.getId());
        return true;
    }

    /**
     * 删除做图任务中的一张图片
     *
     * @param uuid               adi_ai_image uuid
     * @param generatedImageUuid
     * @return
     */
    public boolean delGeneratedFile(String uuid, String generatedImageUuid) {
        AiImage aiImage = checkAndGet(uuid);
        if (StringUtils.isBlank(aiImage.getGeneratedImages())) {
            return false;
        }
        String[] uuids = aiImage.getGeneratedImages().split(",");
        for (int i = 0; i < uuids.length; i++) {
            String fileUuid = uuids[i];
            if (fileUuid.equals(generatedImageUuid)) {
                fileService.removeFileAndSoftDel(fileUuid);
                uuids[i] = "";
            }
        }
        String remainFiles = Arrays.stream(uuids)
                .filter(StringUtils::isNotBlank)
                .collect(Collectors.joining(","));
        _this.lambdaUpdate().eq(AiImage::getId, aiImage.getId()).set(AiImage::getGeneratedImages, remainFiles).update();
        return true;
    }

    private AiImageDto convertAiImageToDto(AiImage aiImage) {
        AiImageDto dto = new AiImageDto();
        BeanUtils.copyProperties(aiImage, dto);
        fillImagesToDto(dto);
        if (StringUtils.isNotBlank(aiImage.getOriginalImage())) {
            dto.setOriginalImageUrl(IMAGE_PATH_PRE + aiImage.getOriginalImage());
        }
        if (StringUtils.isNotBlank(aiImage.getMaskImage())) {
            dto.setMaskImageUrl(IMAGE_PATH_PRE + aiImage.getMaskImage());
        }
        return dto;
    }

    private void fillImagesToDto(AiImageDto aiImageDto) {
        List<String> images = new ArrayList<>();
        aiImageDto.setImageUrlList(images);
        if (StringUtils.isNotBlank(aiImageDto.getGeneratedImages())) {
            String[] imageUuids = aiImageDto.getGeneratedImages().split(",");
            for (String imageUuid : imageUuids) {
                images.add(IMAGE_PATH_PRE + imageUuid);
            }
        }
    }

    private AiImage checkAndGet(String uuid) {
        AiImage aiImage;
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            aiImage = this.lambdaQuery()
                    .eq(AiImage::getUuid, uuid)
                    .eq(AiImage::getIsDeleted, false)
                    .oneOpt()
                    .orElse(null);
        } else {
            aiImage = this.lambdaQuery()
                    .eq(AiImage::getUuid, uuid)
                    .eq(AiImage::getUserId, ThreadContext.getCurrentUserId())
                    .eq(AiImage::getIsDeleted, false)
                    .oneOpt()
                    .orElse(null);
        }
        if (null == aiImage) {
            throw new BaseException(A_AI_IMAGE_NOT_FOUND);
        }
        return aiImage;
    }

    private void softDel(Long id) {
        _this.lambdaUpdate().eq(AiImage::getId, id).set(AiImage::getIsDeleted, true).update();
    }

    public int sumTodayCost() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime begin = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0);
        LocalDateTime end = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 23, 59, 59);
        return this.lambdaQuery()
                .between(AiImage::getCreateTime, begin, end)
                .eq(AiImage::getIsDeleted, false)
                .count()
                .intValue();
    }

    public int sumCurrMonthCost() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime begin = LocalDateTime.of(now.getYear(), now.getMonth(), 1, 0, 0);
        LocalDateTime end = LocalDateTime.of(now.getYear(), now.getMonth(), 1, 23, 59, 59).plusMonths(1).minusDays(1);
        return this.lambdaQuery()
                .between(AiImage::getCreateTime, begin, end)
                .eq(AiImage::getIsDeleted, false)
                .count()
                .intValue();
    }
}
