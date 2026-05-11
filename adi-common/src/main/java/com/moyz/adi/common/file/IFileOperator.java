package com.moyz.adi.common.file;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

public interface IFileOperator {
    boolean checkIfExist(AdiFile adiFile);

    /**
     * @param file     文件 / File
     * @param image    是否图片 / Whether it is an image
     * @param fileName 文件名（不带后缀时根据file获取后缀并追加到fileName） / File name (when no extension, derive from file and append)
     * @return 文件路径及后缀 / File path and extension
     */
    Pair<String, String> save(MultipartFile file, boolean image, String fileName);

    /**
     * @param file     文件 / File
     * @param image    是否图片 / Whether it is an image
     * @param fileName 文件名 / File name
     * @return 文件路径及后缀 / File path and extension
     */
    Pair<String, String> save(byte[] file, boolean image, String fileName);

    SaveRemoteImageResult saveImageFromUrl(String imageUrl, String uuid);

    void delete(AdiFile adiFile);

    String getFileUrl(AdiFile adiFile);

    Document loadDocument(AdiFile adiFile);
}
