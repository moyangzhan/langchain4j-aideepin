package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.FileMapper;
import com.moyz.adi.common.util.FileUtil;
import com.moyz.adi.common.util.MD5Utils;
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
import java.util.UUID;

import static com.moyz.adi.common.enums.ErrorEnum.A_FILE_NOT_EXIST;
import static com.moyz.adi.common.enums.ErrorEnum.B_SAVE_IMAGE_ERROR;

@Slf4j
@Service
public class FileService extends ServiceImpl<FileMapper, AdiFile> {

    @Value("${local.images}")
    private String imagePath;

    @Value("${local.files}")
    private String filePath;

    @Value("${local.tmp-images}")
    private String tmpImagesPath;

    public AdiFile writeToLocal(MultipartFile file) {
        String md5 = MD5Utils.md5ByMultipartFile(file);
        Optional<AdiFile> existFile = this.lambdaQuery()
                .eq(AdiFile::getMd5, md5)
                .eq(AdiFile::getIsDeleted, false)
                .oneOpt();
        if (existFile.isPresent()) {
            return existFile.get();
        }
        String uuid = UUID.randomUUID().toString().replace("-", "");
        Pair<String, String> originalFile = FileUtil.saveToLocal(file, filePath, uuid);
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
        String uuid = UUID.randomUUID().toString().replace("-", "");
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

    public boolean removeFileAndSoftDel(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getUuid, uuid)
                .oneOpt()
                .orElse(null);
        if (null == adiFile) {
            return false;
        }
        if (StringUtils.isNotBlank(adiFile.getPath())) {
            File file = new File(adiFile.getPath());
            file.delete();
        }
        return this.softDel(uuid);
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

    public BufferedImage readBufferedImage(String uuid) {
        AdiFile adiFile = this.lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .oneOpt().orElse(null);
        if (null == adiFile) {
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        try {
            return ImageIO.read(new FileInputStream(adiFile.getPath()));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public String getImagePath(String uuid) {
        return imagePath + uuid + ".png";
    }

    public String getTmpImagesPath(String uuid) {
        return tmpImagesPath + uuid + ".png";
    }
}
