package com.moyz.adi.common.file;

import cn.hutool.core.img.ImgUtil;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.exception.BaseException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;

import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
public class LocalFileUtil {

    public static Pair<String, String> saveToLocal(MultipartFile file, String path, String newName) {
        if (file.isEmpty()) {
            log.info("save to local,file is empty");
            throw new BaseException(A_FILE_NOT_EXIST);
        }
        String fileName = StringUtils.cleanPath(file.getOriginalFilename());
        String fileExt = getFileExtension(fileName);
        log.info("save to local,original name:{},new name:{}", fileName, newName);
        String pathName = path + newName + "." + fileExt;
        try {
            // 将文件保存到目标路径
            file.transferTo(new File(pathName));
        } catch (IOException e) {
            log.error("save to local error", e);
            throw new BaseException(B_SAVE_FILE_ERROR);
        }
        return new ImmutablePair<>(pathName, fileExt);
    }

    /**
     * 读取图片到BufferedImage
     *
     * @param adiFile        图片实体类
     * @param thumbnail      读取的是缩略图
     * @param thumbnailsPath 缩略图路径
     * @return 图片内容
     */
    public static BufferedImage readLocalImage(AdiFile adiFile, boolean thumbnail, String thumbnailsPath) {
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
            log.error("read image error", e);
            throw new BaseException(B_IO_EXCEPTION);
        }
    }

    public static byte[] readBytes(String localPath) {
        try {
            return FileUtils.readFileToByteArray(new File(localPath));
        } catch (NoSuchFileException noSuchFileException) {
            log.error("file not found", noSuchFileException);
            throw new BaseException(A_FILE_NOT_EXIST);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String getFileExtension(String fileName) {
        int dotIndex = fileName.lastIndexOf(".");
        if (dotIndex == -1 || dotIndex == fileName.length() - 1) {
            // 文件名中没有后缀或者后缀位于文件名的末尾
            return "";
        } else {
            int endIndex = fileName.indexOf("?");
            if (endIndex == -1) {
                endIndex = fileName.length();
            }
            return fileName.substring(dotIndex + 1, endIndex);
        }
    }

    public static boolean checkIfExist(String filePath) {
        Path path = Paths.get(filePath);
        return Files.exists(path);
    }

    public static String saveFromUrl(String fileUrl, String newFileName, String defaultExt) {
        String ext = LocalFileUtil.getFileExtension(fileUrl);
        if (org.apache.commons.lang3.StringUtils.isBlank(ext) && !org.apache.commons.lang3.StringUtils.isNotBlank(defaultExt)) {
            ext = defaultExt;
        }
        String filePath = LocalFileOperator.imagePath + newFileName + "." + ext;
        File target = new File(filePath);
        try {
            FileUtils.createParentDirectories(target);
            FileUtils.copyURLToFile(new URL(fileUrl), target);
        } catch (IOException e) {
            log.error("saveToLocal", e);
            throw new BaseException(B_SAVE_IMAGE_ERROR);
        }
        return filePath;
    }
}
