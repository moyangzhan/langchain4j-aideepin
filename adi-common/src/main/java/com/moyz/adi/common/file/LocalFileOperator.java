package com.moyz.adi.common.file;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.loader.FileSystemDocumentLoader;
import dev.langchain4j.data.document.parser.TextDocumentParser;
import dev.langchain4j.data.document.parser.apache.pdfbox.ApachePdfBoxDocumentParser;
import dev.langchain4j.data.document.parser.apache.poi.ApachePoiDocumentParser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.cosntant.AdiConstant.URL_PREFIX_FILE;
import static com.moyz.adi.common.enums.ErrorEnum.*;

@Slf4j
public class LocalFileOperator implements IFileOperator {

    public static String imagePath;

    public static String filePath;

    @Override
    public boolean checkIfExist(AdiFile adiFile) {
        return LocalFileUtil.checkIfExist(adiFile.getPath());
    }

    @Override
    public Pair<String, String> save(MultipartFile file, boolean image, String uuid) {
        return LocalFileUtil.saveToLocal(file, image ? imagePath : filePath, uuid);
    }

    @Override
    public SaveRemoteImageResult saveImageFromUrl(String imageUrl, String uuid) {
        String ext = LocalFileUtil.getFileExtension(imageUrl);
        if (StringUtils.isBlank(ext)) {
            ext = "png";
        }
        String filePath = imagePath + uuid + "." + ext;
        File target = new File(filePath);
        try {
            FileUtils.createParentDirectories(target);
            FileUtils.copyURLToFile(new URL(imageUrl), target);
        } catch (IOException e) {
            log.error("saveToLocal", e);
            throw new BaseException(B_SAVE_IMAGE_ERROR);
        }
        return SaveRemoteImageResult.builder().ext(ext).originalName(target.getName()).pathOrUrl(filePath).build();
    }

    @Override
    public void delete(AdiFile adiFile) {
        if (StringUtils.isBlank(adiFile.getPath())) {
            return;
        }
        try {
            boolean deletedResult = Files.deleteIfExists(Paths.get(adiFile.getPath()));
            if (!deletedResult) {
                log.warn("Delete file fail,uuid:{}", adiFile.getUuid());
            }
        } catch (IOException e) {
            log.error("delete file error", e);
            throw new BaseException(B_DELETE_FILE_ERROR);
        }
    }

    @Override
    public String getFileUrl(AdiFile adiFile) {
        return URL_PREFIX_FILE + adiFile.getUuid() + "." + adiFile.getExt();
    }

    @Override
    public Document loadDocument(AdiFile adiFile) {
        Document result = null;
        String path = adiFile.getPath();
        String ext = adiFile.getExt();
        if (ext.equalsIgnoreCase("txt")) {
            result = FileSystemDocumentLoader.loadDocument(path, new TextDocumentParser());
        } else if (ext.equalsIgnoreCase("pdf")) {
            result = FileSystemDocumentLoader.loadDocument(path, new ApachePdfBoxDocumentParser());
        } else if (ArrayUtils.contains(POI_DOC_TYPES, adiFile.getExt())) {
            result = FileSystemDocumentLoader.loadDocument(path, new ApachePoiDocumentParser());
        }
        return result;
    }

    public static void init(String imagePath, String filePath) {
        LocalFileOperator.imagePath = imagePath;
        LocalFileOperator.filePath = filePath;
    }

    public static boolean checkIfExist(String filePath) {
        Path path = Paths.get(filePath);
        return Files.exists(path);
    }

    public static boolean checkAndCreateDir(String dir) {
        Path dirPath = Paths.get(dir);
        if (!Files.exists(dirPath)) {
            try {
                // 创建目录，包括所有不存在的父目录
                Files.createDirectories(dirPath);
            } catch (IOException e) {
                System.err.println("创建目录时发生错误：" + e.getMessage());
                throw new BaseException(B_DIR_CREATE_FAIL);
            }
        }
        return true;
    }
}
