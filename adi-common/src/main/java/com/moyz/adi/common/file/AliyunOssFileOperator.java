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
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.enums.ErrorEnum.B_DELETE_FILE_ERROR;

@Slf4j
public class AliyunOssFileOperator implements IFileOperator {

    private static AliyunOssFileHelper aliyunOssFileHelper;

    @Override
    public boolean checkIfExist(AdiFile adiFile) {
        return aliyunOssFileHelper.doesObjectExist(getObjectName(adiFile));
    }

    @Override
    public Pair<String, String> save(MultipartFile file, boolean image, String fileName) {
        String objectName;
        String ext;
        if (fileName.contains(".")) {
            ext = LocalFileUtil.getFileExtension(fileName);
        } else {
            ext = LocalFileUtil.getFileExtension(file.getOriginalFilename());
        }
        objectName = fileName + "." + ext;
        try {
            aliyunOssFileHelper.saveObj(file.getBytes(), objectName);
        } catch (IOException e) {
            throw new BaseException(B_DELETE_FILE_ERROR);
        }
        return new ImmutablePair<>(aliyunOssFileHelper.getUrl(objectName), ext);
    }

    @Override
    public Pair<String, String> save(byte[] file, boolean image, String name) {
        String ext = LocalFileUtil.getFileExtension(name);
        aliyunOssFileHelper.saveObj(file, name);
        return new ImmutablePair<>(aliyunOssFileHelper.getUrl(name), ext);
    }

    @Override
    public SaveRemoteImageResult saveImageFromUrl(String imageUrl, String fileName) {
        String filePath = LocalFileUtil.saveFromUrl(imageUrl, fileName, "png");
        byte[] bytes = LocalFileUtil.readBytes(filePath);
        String ext = LocalFileUtil.getFileExtension(filePath);
        String objName = fileName + "." + ext;
        aliyunOssFileHelper.saveObj(bytes, objName);
        try {
            //传到oss后把本地临时文件删除
            Files.deleteIfExists(Paths.get(filePath));
        } catch (IOException e) {
            throw new BaseException(B_DELETE_FILE_ERROR);
        }
        //对于OSS，存储的是对象名称，而不是完整URL
        filePath = objName;
        return SaveRemoteImageResult.builder().ext(ext).originalName(fileName).pathOrUrl(filePath).build();
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
