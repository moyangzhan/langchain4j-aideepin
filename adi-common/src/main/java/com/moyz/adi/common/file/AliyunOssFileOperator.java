package com.moyz.adi.common.file;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.loader.UrlDocumentLoader;
import dev.langchain4j.data.document.parser.TextDocumentParser;
import dev.langchain4j.data.document.parser.apache.pdfbox.ApachePdfBoxDocumentParser;
import dev.langchain4j.data.document.parser.apache.poi.ApachePoiDocumentParser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.enums.ErrorEnum.B_DELETE_FILE_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_SAVE_IMAGE_ERROR;

@Slf4j
public class AliyunOssFileOperator implements IFileOperator {

    private static AliyunOssFileHelper aliyunOssFileHelper;

    @Override
    public boolean checkIfExist(AdiFile adiFile) {
        return aliyunOssFileHelper.doesObjectExist(getObjectName(adiFile));
    }

    @Override
    public Pair<String, String> save(MultipartFile file, boolean image, String uuid) {
        Pair<String, String> pathAndExt = LocalFileUtil.saveToLocal(file, image ? LocalFileOperator.imagePath : LocalFileOperator.filePath, uuid);
        String objectName = uuid + "." + pathAndExt.getRight();
        byte[] bytes = LocalFileUtil.readBytes(pathAndExt.getLeft());
        aliyunOssFileHelper.saveObj(bytes, objectName);
        try {
            //传到oss后把本地临时文件删除
            Files.deleteIfExists(Paths.get(pathAndExt.getLeft()));
        } catch (IOException e) {
            throw new BaseException(B_DELETE_FILE_ERROR);
        }
        return new ImmutablePair<>(aliyunOssFileHelper.getUrl(objectName), pathAndExt.getRight());
    }

    @Override
    public SaveRemoteImageResult saveImageFromUrl(String imageUrl, String uuid) {
        String ext = LocalFileUtil.getFileExtension(imageUrl);
        if (StringUtils.isBlank(ext)) {
            ext = "png";
        }
        String filePath = LocalFileOperator.imagePath + uuid + "." + ext;
        File target = new File(filePath);
        try {
            FileUtils.createParentDirectories(target);
            FileUtils.copyURLToFile(new URL(imageUrl), target);
        } catch (IOException e) {
            log.error("saveToLocal", e);
            throw new BaseException(B_SAVE_IMAGE_ERROR);
        }
        byte[] bytes = LocalFileUtil.readBytes(filePath);
        String objName = uuid + "." + ext;
        aliyunOssFileHelper.saveObj(bytes, objName);
        try {
            //传到oss后把本地临时文件删除
            Files.deleteIfExists(Paths.get(filePath));
        } catch (IOException e) {
            throw new BaseException(B_DELETE_FILE_ERROR);
        }
        //对于OSS，存储的是对象名称，而不是完整URL
        filePath = objName;
        return SaveRemoteImageResult.builder().ext(ext).originalName(target.getName()).pathOrUrl(filePath).build();
    }

    @Override
    public void delete(AdiFile adiFile) {
        if (StringUtils.isBlank(adiFile.getPath())) {
            return;
        }
        aliyunOssFileHelper.deleteObjs(List.of(getObjectName(adiFile)));
    }

    @Override
    public String getFileUrl(AdiFile adiFile) {
        return aliyunOssFileHelper.getUrl(getObjectName(adiFile));
    }

    @Override
    public Document loadDocument(AdiFile adiFile) {
        Document result = null;
        String path = adiFile.getPath();
        String ext = adiFile.getExt();
        if (ext.equalsIgnoreCase("txt")) {
            result = UrlDocumentLoader.load(path, new TextDocumentParser());
        } else if (ext.equalsIgnoreCase("pdf")) {
            result = UrlDocumentLoader.load(path, new ApachePdfBoxDocumentParser());
        } else if (ArrayUtils.contains(POI_DOC_TYPES, adiFile.getExt())) {
            result = UrlDocumentLoader.load(path, new ApachePoiDocumentParser());
        }
        return result;
    }

    public static String getObjectName(AdiFile adiFile) {
        return adiFile.getUuid() + "." + adiFile.getExt();
    }

    public static void init(AliyunOssFileHelper aliyunOssFileHelper) {
        AliyunOssFileOperator.aliyunOssFileHelper = aliyunOssFileHelper;
    }
}
