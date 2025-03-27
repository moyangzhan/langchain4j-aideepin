package com.moyz.adi.common.file;

import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

public interface IFileOperator {
    boolean checkIfExist(AdiFile adiFile);
    Pair<String, String> save(MultipartFile file, boolean image, String uuid);
    SaveRemoteImageResult saveImageFromUrl(String imageUrl, String uuid);
    void delete(AdiFile adiFile);
    String getFileUrl(AdiFile adiFile);
    Document loadDocument(AdiFile adiFile);
}
