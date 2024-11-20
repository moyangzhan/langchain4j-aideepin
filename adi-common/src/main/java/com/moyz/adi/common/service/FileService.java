package com.moyz.adi.common.service;

import cn.hutool.core.img.ImgUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.FileMapper;
import com.moyz.adi.common.util.FileUtil;
import com.moyz.adi.common.util.MD5Utils;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Optional;

import static com.moyz.adi.common.enums.ErrorEnum.*;

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

    public AdiFile writeToLocal(MultipartFile file, boolean image) {
        String md5 = MD5Utils.md5ByMultipartFile(file);
        Optional<AdiFile> existFile = this.lambdaQuery()
                .eq(AdiFile::getMd5, md5)
                .eq(AdiFile::getIsDeleted, false)
                .oneOpt();
        if (existFile.isPresent()) {
            AdiFile adiFile = existFile.get();
            boolean exist = FileUtil.checkIfExist(adiFile.getPath());
            if (exist) {
                return adiFile;
            } else {
                log.warn("文件不存在,删除记录以便后续重新生成,fileId:{},uuid:{},md5:{}", adiFile.getId(), adiFile.getUuid(), adiFile.getMd5());
                this.lambdaUpdate().eq(AdiFile::getId, adiFile.getId()).set(AdiFile::getIsDeleted, true).update();
            }
        }
        String uuid = UuidUtil.createShort();
        Pair<String, String> originalFile = FileUtil.saveToLocal(file, image ? imagePath : filePath, uuid);
        AdiFile adiFile = new AdiFile();
        adiFile.setName(file.getOriginalFilename());
        adiFile.setUuid(uuid);
        adiFile.setMd5(md5);
        adiFile.setPath(originalFile.getLeft());
        adiFile.setExt(originalFile.getRight());
        adiFile.setUserId(ThreadContext.getCurrentUserId());
        this.getBaseMapper().insert(adiFile);
        return adiFile;
    }

    public String saveImageToLocal(User user, String sourceImageUrl) {
        String uuid = UuidUtil.createShort();
        String localPath = imagePath + uuid + ".png";
        File target = new File(localPath);
        try {
            FileUtils.createParentDirectories(target);
            FileUtils.copyURLToFile(new URL(sourceImageUrl), target);
        } catch (IOException e) {
            log.error("saveToLocal", e);
            throw new BaseException(B_SAVE_IMAGE_ERROR);
        }
        AdiFile adiFile = new AdiFile();
        adiFile.setName(target.getName());
        adiFile.setUuid(uuid);
        adiFile.setMd5(MD5Utils.calculateMD5(localPath));
        adiFile.setPath(localPath);
        adiFile.setUserId(user.getId());
        adiFile.setExt("png");
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

    public void removeFileAndSoftDel(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getUuid, uuid)
                .oneOpt()
                .orElse(null);
        if (null == adiFile) {
            return;
        }
        if (StringUtils.isNotBlank(adiFile.getPath())) {
            File file = new File(adiFile.getPath());
            if (!file.delete()) {
                log.warn("Delete file error,uuid:{}", uuid);
            }
        }
        this.softDel(uuid);
    }

    public AdiFile getByUuid(String uuid) {
        return this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt().orElse(null);
    }

    public byte[] readBytes(AdiFile adiFile) {
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        return FileUtil.readBytes(adiFile.getPath());
    }

    public byte[] readBytes(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        return FileUtil.readBytes(adiFile.getPath());
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
        return readImage(adiFile, thumbnail);
    }

    public BufferedImage readImage(String uuid, boolean thumbnail) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        return readImage(adiFile, thumbnail);
    }

    /**
     * 读取图片到BufferedImage
     *
     * @param adiFile   图片实体类
     * @param thumbnail 读取的是缩略图
     * @return 图片内容
     */
    private BufferedImage readImage(AdiFile adiFile, boolean thumbnail) {
        try {
            String currentFilePath = adiFile.getPath();
            if (thumbnail) {
                currentFilePath = thumbnailsPath + adiFile.getUuid() + "." + adiFile.getExt();
                //不存在则创建
                if (new File(adiFile.getPath()).exists() && !new File(currentFilePath).exists()) {
                    ImgUtil.scale(
                            cn.hutool.core.io.FileUtil.file(adiFile.getPath()),
                            cn.hutool.core.io.FileUtil.file(currentFilePath),
                            0.2f
                    );
                }
            }
            return ImageIO.read(new FileInputStream(currentFilePath));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
}
