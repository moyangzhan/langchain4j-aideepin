package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.AdiFileHelper;
import com.moyz.adi.common.mapper.FileMapper;
import com.moyz.adi.common.util.HashUtil;
import com.moyz.adi.common.util.LocalFileUtil;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static com.moyz.adi.common.enums.ErrorEnum.A_AI_IMAGE_NO_AUTH;
import static com.moyz.adi.common.enums.ErrorEnum.A_FILE_NOT_EXIST;

@Slf4j
@Service
public class FileService extends ServiceImpl<FileMapper, AdiFile> {

    @Value("${local.images}")
    private String imagePath;

    @Value("${local.watermark-images}")
    private String watermarkImagesPath;

    @Value("${local.thumbnails}")
    private String thumbnailsPath;

    @Value("${local.watermark-thumbnails}")
    private String watermarkThumbnailsPath;

    @Value("${local.files}")
    private String filePath;

    @Value("${local.tmp-images}")
    private String tmpImagesPath;

    @Resource
    private AdiFileHelper adiFileHelper;

    public AdiFile saveFile(MultipartFile file, boolean image) {
        String sha256 = HashUtil.sha256(file);
        Optional<AdiFile> existFile = this.lambdaQuery()
                .eq(AdiFile::getSha256, sha256)
                .eq(AdiFile::getIsDeleted, false)
                .oneOpt();
        if (existFile.isPresent()) {
            AdiFile adiFile = existFile.get();
            boolean exist = adiFileHelper.checkIfExist(adiFile);
            if (exist) {
                return adiFile;
            } else {
                log.warn("文件不存在,删除记录以便后续重新生成,fileId:{},uuid:{},sha256:{}", adiFile.getId(), adiFile.getUuid(), adiFile.getSha256());
                this.lambdaUpdate().eq(AdiFile::getId, adiFile.getId()).set(AdiFile::getIsDeleted, true).update();
            }
        }
        String uuid = UuidUtil.createShort();
        Pair<String, String> originalFile = adiFileHelper.save(file, image, uuid);
        AdiFile adiFile = new AdiFile();
        adiFile.setName(file.getOriginalFilename());
        adiFile.setUuid(uuid);
        adiFile.setSha256(sha256);
        adiFile.setPath(originalFile.getLeft());
        adiFile.setExt(originalFile.getRight());
        adiFile.setUserId(ThreadContext.getCurrentUserId());
        adiFile.setStorageLocation(AdiFileHelper.getStorageLocation());
        this.getBaseMapper().insert(adiFile);
        return adiFile;
    }

    public String saveImageFromUrl(User user, String sourceImageUrl) {
        log.info("saveImageFromUrl,sourceImageUrl:{}", sourceImageUrl);
        String uuid = UuidUtil.createShort();
        SaveRemoteImageResult saveResult = adiFileHelper.saveImageFromUrl(sourceImageUrl, uuid);
        AdiFile adiFile = new AdiFile();
        adiFile.setName(saveResult.getOriginalName());
        adiFile.setUuid(uuid);
        adiFile.setSha256(HashUtil.sha256(saveResult.getPathOrUrl()));
        adiFile.setPath(saveResult.getPathOrUrl());
        adiFile.setUserId(user.getId());
        adiFile.setExt("png");
        adiFile.setStorageLocation(AdiFileHelper.getStorageLocation());
        this.getBaseMapper().insert(adiFile);
        return uuid;
    }

    public boolean softDel(String uuid) {
        return this.lambdaUpdate()
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getUuid, uuid)
                .set(AdiFile::getIsDeleted, true)
                .update();
    }

    public boolean removeFileAndSoftDel(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getUuid, uuid)
                .oneOpt()
                .orElse(null);
        if (null == adiFile) {
            return false;
        }
        adiFileHelper.delete(adiFile);
        return this.softDel(uuid);
    }

    public AdiFile getByUuid(String uuid) {
        return this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt().orElse(null);
    }

    /**
     * 读取图片到BufferedImage，管理员或图片拥有者才有权限查看
     *
     * @param uuid      图片uuid
     * @param thumbnail 读取的是缩略图
     * @return 图片内容
     */
    public BufferedImage readMyImage(String uuid, boolean thumbnail) {
        if (StringUtils.isBlank(ThreadContext.getToken())) {
            throw new BaseException(A_AI_IMAGE_NO_AUTH);
        }
        AdiFile adiFile = this.lambdaQuery()
                .eq(!ThreadContext.getCurrentUser().getIsAdmin(), AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        return LocalFileUtil.readLocalImage(adiFile, thumbnail, thumbnailsPath);
    }

    public BufferedImage readImage(String uuid, boolean thumbnail) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        return LocalFileUtil.readLocalImage(adiFile, thumbnail, thumbnailsPath);
    }

    public String getImagePath(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(ErrorEnum.A_AI_IMAGE_NOT_FOUND);
        }
        return adiFile.getPath();
    }


    public AdiFile getFile(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(ErrorEnum.A_AI_IMAGE_NOT_FOUND);
        }
        return adiFile;
    }

    public String getTmpImagesPath(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(ErrorEnum.A_AI_IMAGE_NOT_FOUND);
        }
        return tmpImagesPath + uuid + "." + adiFile.getExt();
    }

    public String getWatermarkImagesPath(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(ErrorEnum.A_AI_IMAGE_NOT_FOUND);
        }
        return watermarkImagesPath + uuid + "." + adiFile.getExt();
    }

    public String getWatermarkImagesPath(AdiFile adiFile) {
        return watermarkImagesPath + adiFile.getUuid() + "." + adiFile.getExt();
    }

    public String getUrl(String fileUuid) {
        if (StringUtils.isBlank(fileUuid)) {
            return null;
        }
        List<String> list = getUrls(List.of(fileUuid));
        if (!list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }

    /**
     * 获取文件url
     *
     * @param fileUuids 文件uuid
     * @return 文件url
     */
    public List<String> getUrls(List<String> fileUuids) {
        if (CollectionUtils.isEmpty(fileUuids)) {
            return Collections.emptyList();
        }
        List<String> result = new ArrayList<>();
        this.lambdaQuery()
                .in(AdiFile::getUuid, fileUuids)
                .eq(AdiFile::getIsDeleted, false)
                .list()
                .forEach(adiFile -> {
                    result.add(adiFileHelper.getFileUrl(adiFile));
                });
        return result;
    }
}
