package com.moyz.adi.common.service;

import cn.hutool.core.img.Img;
import cn.hutool.core.io.FileUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.*;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.ImageModelContext;
import com.moyz.adi.common.helper.QuotaHelper;
import com.moyz.adi.common.helper.RateLimitHelper;
import com.moyz.adi.common.mapper.DrawMapper;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.awt.*;
import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.List;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.GenerateImage.*;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
@Service
public class DrawService extends ServiceImpl<DrawMapper, Draw> {

    @Resource
    @Lazy
    private DrawService _this;

    @Value("${local.images}")
    private String imagePath;

    @Value("${local.watermark-images}")
    private String watermarkImagesPath;

    @Value("${local.thumbnails}")
    private String thumbnailsPath;

    @Value("${local.watermark-thumbnails}")
    private String watermarkThumbnailsPath;

    @Value("${adi.host}")
    private String adiHost;

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

    @Resource
    private AiModelService aiModelService;

    @Resource
    private DrawStarService drawStarService;

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
     * @param generateImageReq 文生图请求参数
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
        createImageDto.setModelName(editImageReq.getModelName());
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
        AiModel aiModel = aiModelService.getByNameOrThrow(createImageDto.getModelName());
        User user = ThreadContext.getCurrentUser();
        int generateNumber = Math.min(createImageDto.getNumber(), user.getQuotaByImageDaily());
        String uuid = UuidUtil.createShort();
        Draw draw = new Draw();
        draw.setGenerateSize(createImageDto.getSize());
        draw.setGenerateQuality(createImageDto.getQuality());
        draw.setGenerateNumber(generateNumber);
        draw.setUuid(uuid);
        draw.setAiModelId(aiModel.getId());
        draw.setAiModelName(createImageDto.getModelName());
        draw.setUserId(user.getId());
        draw.setInteractingMethod(createImageDto.getInteractingMethod());
        draw.setProcessStatus(STATUS_DOING);
        draw.setPrompt(createImageDto.getPrompt());
        draw.setOriginalImage(createImageDto.getOriginalImage());
        draw.setMaskImage(createImageDto.getMaskImage());
        getBaseMapper().insert(draw);
        Draw obj = this.lambdaQuery().eq(Draw::getUuid, uuid).one();
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
        Draw obj = this.lambdaQuery()
                .eq(Draw::getUuid, uuid)
                .eq(Draw::getProcessStatus, STATUS_FAIL)
                .oneOpt().orElseThrow(() -> new BaseException(B_FIND_IMAGE_404));

        _this.createFromRemote(obj, user);
    }

    /**
     * 异步生成图片
     *
     * @param draw
     * @param user
     */
    @Async("imagesExecutor")
    public void createFromRemote(Draw draw, User user) {
        String drawingKey = MessageFormat.format(RedisKeyConstant.USER_DRAWING, user.getId());
        stringRedisTemplate.opsForValue().set(drawingKey, "1", 30, TimeUnit.SECONDS);

        try {
            //Increase the number of the request
            String requestTimesKey = MessageFormat.format(RedisKeyConstant.USER_REQUEST_TEXT_TIMES, user.getId());
            rateLimitHelper.increaseRequestTimes(requestTimesKey, LocalCache.IMAGE_RATE_LIMIT_CONFIG);

            List<String> images = new ArrayList<>();
            if (draw.getInteractingMethod() == INTERACTING_METHOD_GENERATE_IMAGE) {
                images = ImageModelContext.getModelService(draw.getAiModelName()).generateImage(user, draw);
            } else if (draw.getInteractingMethod() == INTERACTING_METHOD_EDIT_IMAGE) {
                images = ImageModelContext.getModelService(draw.getAiModelName()).editImage(user, draw);
            } else if (draw.getInteractingMethod() == INTERACTING_METHOD_VARIATION) {
                images = ImageModelContext.getModelService(draw.getAiModelName()).createImageVariation(user, draw);
            }
            List<String> imageUuids = new ArrayList<>();
            images.forEach(imageUrl -> {
                String imageUuid = fileService.saveImageToLocal(user, imageUrl);
                imageUuids.add(imageUuid);
            });
            String imageUuidsJoin = imageUuids.stream().collect(Collectors.joining(","));
            if (StringUtils.isBlank(imageUuidsJoin)) {
                _this.lambdaUpdate().eq(Draw::getId, draw.getId()).set(Draw::getProcessStatus, STATUS_FAIL).update();
                return;
            }
            String respImagesPath = images.stream().collect(Collectors.joining(","));
            updateDrawStatus(draw.getId(), respImagesPath, imageUuidsJoin, STATUS_SUCCESS);

            //Update the cost of current user
            UserDayCost userDayCost = userDayCostService.getTodayCost(user);
            UserDayCost saveOrUpdateInst = new UserDayCost();
            if (null == userDayCost) {
                saveOrUpdateInst.setUserId(user.getId());
                saveOrUpdateInst.setDay(LocalDateTimeUtil.getToday());
                saveOrUpdateInst.setImagesNumber(images.size());
            } else {
                saveOrUpdateInst.setId(userDayCost.getId());
                saveOrUpdateInst.setImagesNumber(userDayCost.getImagesNumber() + images.size());
            }
            userDayCostService.saveOrUpdate(saveOrUpdateInst);
        } finally {
            stringRedisTemplate.delete(drawingKey);
        }
    }

    public void updateDrawStatus(Long drawId, String respImagesPath, String localImagesUuid, int generationStatus) {
        Draw updateImage = new Draw();
        updateImage.setId(drawId);
        updateImage.setRespImagesPath(respImagesPath);
        updateImage.setGeneratedImages(localImagesUuid);
        updateImage.setProcessStatus(generationStatus);
        getBaseMapper().updateById(updateImage);

        AdiFile adiFile = fileService.lambdaQuery().eq(AdiFile::getUuid, localImagesUuid).oneOpt().orElse(null);
        if (null != adiFile) {
            fileService.lambdaUpdate().eq(AdiFile::getId, adiFile.getId()).set(AdiFile::getRefCount, adiFile.getRefCount() + 1).update();
        }
    }

    public DrawListResp listByCurrentUser(Long maxId, int pageSize) {
        List<Draw> list = this.lambdaQuery()
                .eq(Draw::getUserId, ThreadContext.getCurrentUserId())
                .eq(Draw::getIsDeleted, false)
                .lt(Draw::getId, maxId)
                .orderByDesc(Draw::getId)
                .last("limit " + pageSize)
                .list();
        list.sort(Comparator.comparing(Draw::getId));
        return imagesToListResp(list);
    }

    /**
     * 倒序查询公开的图片
     *
     * @param maxId    最大的ID
     * @param pageSize 每次请示获取的数量
     * @return 图片列表
     */
    public DrawListResp listPublic(Long maxId, int pageSize) {
        List<Draw> list = this.lambdaQuery()
                .eq(Draw::getIsDeleted, false)
                .eq(Draw::getIsPublic, true)
                .lt(Draw::getId, maxId)
                .orderByDesc(Draw::getId)
                .last("limit " + pageSize)
                .list();
        return imagesToListResp(list);
    }

    public DrawListResp listFav(Long maxId, int pageSize) {
        List<DrawStar> stars = drawStarService.listByCurrentUser(maxId, pageSize);
        if (CollectionUtils.isEmpty(stars)) {
            DrawListResp resp = new DrawListResp();
            resp.setDraws(Collections.emptyList());
            resp.setMinId(Long.MAX_VALUE);
            return resp;
        }
        List<Draw> list = this.lambdaQuery()
                .in(Draw::getId, stars.stream().map(DrawStar::getDrawId).toList())
                .list();
        return imagesToListResp(list);
    }

    private DrawListResp imagesToListResp(List<Draw> list) {
        List<DrawDto> dtoList = new ArrayList<>();
        list.forEach(item -> dtoList.add(convertDrawToDto(item)));
        DrawListResp result = new DrawListResp();
        result.setDraws(dtoList);
        result.setMinId(list.stream().map(Draw::getId).reduce(Long.MAX_VALUE, Long::min));
        return result;
    }

    public DrawDto getOne(String uuid) {
        Draw draw = this.lambdaQuery()
                .eq(Draw::getUuid, uuid)
                .eq(Draw::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt()
                .orElse(null);
        if (null != draw) {
            return convertDrawToDto(draw);
        } else {
            return null;
        }
    }

    public DrawDto getOrThrow(String uuid) {
        Draw draw = this.lambdaQuery()
                .eq(Draw::getUuid, uuid)
                .eq(Draw::getUserId, ThreadContext.getCurrentUserId())
                .one();
        if (null == draw) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        return convertDrawToDto(draw);
    }

    public DrawDto getPublicOne(String uuid) {
        Draw draw = this.lambdaQuery()
                .eq(Draw::getUuid, uuid)
                .eq(Draw::getIsDeleted, false)
                .eq(Draw::getIsPublic, true)
                .oneOpt()
                .orElse(null);
        if (null != draw) {
            return convertDrawToDto(draw);
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
        Draw draw = checkAndGet(uuid);
        if (StringUtils.isNotBlank(draw.getGeneratedImages())) {
            String[] uuids = draw.getGeneratedImages().split(",");
            for (String fileUuid : uuids) {
                fileService.removeFileAndSoftDel(fileUuid);
            }
        }
        _this.softDel(draw.getId());
        return true;
    }

    /**
     * 删除做图任务中的一张图片
     *
     * @param uuid               adi_draw uuid
     * @param generatedImageUuid 图片uuid
     * @return 是否成功
     */
    public boolean delGeneratedFile(String uuid, String generatedImageUuid) {
        Draw draw = checkAndGet(uuid);
        if (StringUtils.isBlank(draw.getGeneratedImages())) {
            return false;
        }
        String[] uuids = draw.getGeneratedImages().split(",");
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
        _this.lambdaUpdate().eq(Draw::getId, draw.getId()).set(Draw::getGeneratedImages, remainFiles).update();
        return true;
    }

    private DrawDto convertDrawToDto(Draw draw) {
        DrawDto dto = new DrawDto();
        BeanUtils.copyProperties(draw, dto);
        fillImagesToDto(dto);
        if (StringUtils.isNotBlank(draw.getOriginalImage())) {
            dto.setOriginalImageUuid(draw.getOriginalImage());
        }
        if (StringUtils.isNotBlank(draw.getMaskImage())) {
            dto.setMaskImageUuid(draw.getMaskImage());
        }
        return dto;
    }

    private void fillImagesToDto(DrawDto drawDto) {
        List<String> images = new ArrayList<>();
        if (StringUtils.isNotBlank(drawDto.getGeneratedImages())) {
            String[] imageUuids = drawDto.getGeneratedImages().split(",");
            images.addAll(Arrays.asList(imageUuids));
        }
        drawDto.setImageUuids(images);
    }

    private Draw checkAndGet(String uuid) {
        Draw draw;
        if (Boolean.TRUE.equals(ThreadContext.getCurrentUser().getIsAdmin())) {
            draw = this.lambdaQuery()
                    .eq(Draw::getUuid, uuid)
                    .eq(Draw::getIsDeleted, false)
                    .oneOpt()
                    .orElse(null);
        } else {
            draw = this.lambdaQuery()
                    .eq(Draw::getUuid, uuid)
                    .eq(Draw::getUserId, ThreadContext.getCurrentUserId())
                    .eq(Draw::getIsDeleted, false)
                    .oneOpt()
                    .orElse(null);
        }
        if (null == draw) {
            throw new BaseException(A_AI_IMAGE_NOT_FOUND);
        }
        return draw;
    }

    private void softDel(Long id) {
        this.lambdaUpdate().eq(Draw::getId, id).set(Draw::getIsDeleted, true).update();
    }

    public int sumTodayCost() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime begin = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0);
        LocalDateTime end = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 23, 59, 59);
        return this.lambdaQuery()
                .between(Draw::getCreateTime, begin, end)
                .eq(Draw::getIsDeleted, false)
                .count()
                .intValue();
    }

    public int sumCurrMonthCost() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime begin = LocalDateTime.of(now.getYear(), now.getMonth(), 1, 0, 0);
        LocalDateTime end = LocalDateTime.of(now.getYear(), now.getMonth(), 1, 23, 59, 59).plusMonths(1).minusDays(1);
        return this.lambdaQuery()
                .between(Draw::getCreateTime, begin, end)
                .eq(Draw::getIsDeleted, false)
                .count()
                .intValue();
    }

    public void setImagePublic(String uuid, Boolean isPublic, Boolean withWatermark) {
        Draw draw = checkAndGet(uuid);
        //生成水印
        if (BooleanUtils.isTrue(withWatermark)) {
            AdiFile adiFile = fileService.getFile(uuid);
            String markImagePath = fileService.getWatermarkImagesPath(adiFile);
            if (!FileUtil.exist(markImagePath)) {
                Img.from(FileUtil.file(adiFile.getPath())).setPositionBaseCentre(false).pressText(
                        ThreadContext.getCurrentUser().getName() + "|" + adiHost, Color.WHITE,
                        null,
                        0,
                        0,
                        0.4f);
            }
        }
        this.lambdaUpdate()
                .eq(Draw::getId, draw.getId())
                .set(Draw::getIsPublic, isPublic)
                .set(BooleanUtils.isTrue(withWatermark), Draw::getWithWatermark, withWatermark)
                .update();
    }
}
