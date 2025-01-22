package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.util.LocalFileUtil;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.loader.FileSystemDocumentLoader;
import dev.langchain4j.data.document.loader.UrlDocumentLoader;
import dev.langchain4j.data.document.parser.TextDocumentParser;
import dev.langchain4j.data.document.parser.apache.pdfbox.ApachePdfBoxDocumentParser;
import dev.langchain4j.data.document.parser.apache.poi.ApachePoiDocumentParser;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.cosntant.AdiConstant.URL_PREFIX_FILE;
import static com.moyz.adi.common.enums.ErrorEnum.B_DELETE_FILE_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.B_SAVE_IMAGE_ERROR;
import static dev.langchain4j.data.document.loader.FileSystemDocumentLoader.loadDocument;

/**
 * 文件操作封装类
 */
@Slf4j
@Service
public class AdiFileHelper {

    @Value("${local.images}")
    private String imagePath;

    @Value("${local.files}")
    private String filePath;

    @Resource
    private AliyunOssHelper aliyunOssHelper;

    public boolean checkIfExist(AdiFile adiFile) {
        if (adiFile.getStorageLocation() == AdiConstant.STORAGE_LOCATION_LOCAL) {
            return LocalFileUtil.checkIfExist(adiFile.getPath());
        } else {
            return aliyunOssHelper.doesObjectExist(getObjectName(adiFile));
        }
    }

    public Pair<String, String> save(MultipartFile file, boolean image, String uuid) {
        Pair<String, String> pathAndExt = LocalFileUtil.saveToLocal(file, image ? imagePath : filePath, uuid);
        if (AdiConstant.STORAGE_LOCATION_LOCAL == getStorageLocation()) {
            return pathAndExt;
        } else {
            String objectName = uuid + "." + pathAndExt.getRight();
            byte[] bytes = LocalFileUtil.readBytes(pathAndExt.getLeft());
            aliyunOssHelper.saveObj(bytes, objectName);
            try {
                //传到oss后把本地临时文件删除
                Files.deleteIfExists(Paths.get(pathAndExt.getLeft()));
            } catch (IOException e) {
                throw new BaseException(B_DELETE_FILE_ERROR);
            }
            return new ImmutablePair<>(aliyunOssHelper.getUrl(objectName), pathAndExt.getRight());
        }
    }

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
        if (AdiConstant.STORAGE_LOCATION_ALI_OSS == getStorageLocation()) {
            byte[] bytes = LocalFileUtil.readBytes(filePath);
            String objName = uuid + "." + ext;
            aliyunOssHelper.saveObj(bytes, objName);
            try {
                //传到oss后把本地临时文件删除
                Files.deleteIfExists(Paths.get(filePath));
            } catch (IOException e) {
                throw new BaseException(B_DELETE_FILE_ERROR);
            }
            //对于OSS，存储的是对象名称，而不是完整URL
            filePath = objName;
        }
        return SaveRemoteImageResult.builder().ext(ext).originalName(target.getName()).pathOrUrl(filePath).build();
    }

    public void delete(AdiFile adiFile) {
        if (StringUtils.isBlank(adiFile.getPath())) {
            return;
        }
        try {
            if (adiFile.getStorageLocation() == AdiConstant.STORAGE_LOCATION_LOCAL) {
                boolean deletedResult = Files.deleteIfExists(Paths.get(adiFile.getPath()));
                if (!deletedResult) {
                    log.warn("Delete file fail,uuid:{}", adiFile.getUuid());
                }
            } else {
                aliyunOssHelper.deleteObjs(List.of(getObjectName(adiFile)));
            }
        } catch (IOException e) {
            throw new BaseException(B_DELETE_FILE_ERROR);
        }

    }

    public static String getObjectName(AdiFile adiFile) {
        return adiFile.getUuid() + "." + adiFile.getExt();
    }

    public static int getStorageLocation() {
        return SysConfigService.getIntByKey(AdiConstant.SysConfigKey.STORAGE_LOCATION, -1);
    }

    public String getStorageHost(int location) {
        if (location == AdiConstant.STORAGE_LOCATION_ALI_OSS) {
            return aliyunOssHelper.getConfigObj().getEndpoint();
        }
        return null;
    }

    public String getFileUrl(AdiFile adiFile) {
        if (adiFile.getStorageLocation() == AdiConstant.STORAGE_LOCATION_LOCAL) {
            return URL_PREFIX_FILE + adiFile.getUuid();
        } else {
            //TODO OSS对外开放的路径换成预签名URL 或 当关闭外部访问权限时（如绘图任务设置为私有），更换文件名（即换成新UUID）
            //需要在OSS控制台设置跨域访问权限
            return aliyunOssHelper.getUrl(getObjectName(adiFile));
        }
    }

    public Document loadDocument(AdiFile adiFile) {
        Document result = null;
        String path = adiFile.getPath();
        String ext = adiFile.getExt();
        int storageLocation = adiFile.getStorageLocation();
        if (storageLocation == AdiConstant.STORAGE_LOCATION_LOCAL) {
            if (ext.equalsIgnoreCase("txt")) {
                result = FileSystemDocumentLoader.loadDocument(path, new TextDocumentParser());
            } else if (ext.equalsIgnoreCase("pdf")) {
                result = FileSystemDocumentLoader.loadDocument(path, new ApachePdfBoxDocumentParser());
            } else if (ArrayUtils.contains(POI_DOC_TYPES, adiFile.getExt())) {
                result = FileSystemDocumentLoader.loadDocument(path, new ApachePoiDocumentParser());
            }
        } else {
            if (ext.equalsIgnoreCase("txt")) {
                result = UrlDocumentLoader.load(path, new TextDocumentParser());
            } else if (ext.equalsIgnoreCase("pdf")) {
                result = UrlDocumentLoader.load(path, new ApachePdfBoxDocumentParser());
            } else if (ArrayUtils.contains(POI_DOC_TYPES, adiFile.getExt())) {
                result = UrlDocumentLoader.load(path, new ApachePoiDocumentParser());
            }
        }
        return result;
    }
}
