package com.moyz.adi.common.file;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.vo.SaveRemoteImageResult;
import dev.langchain4j.data.document.Document;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.web.multipart.MultipartFile;

import java.util.HashMap;
import java.util.Map;

import static com.moyz.adi.common.cosntant.AdiConstant.STORAGE_LOCATION_ALI_OSS;
import static com.moyz.adi.common.cosntant.AdiConstant.STORAGE_LOCATION_LOCAL;

public class FileOperatorContext {

    private static final Map<Integer, IFileOperator> CONCRETE_OPT = new HashMap<>();

    static {
        CONCRETE_OPT.put(STORAGE_LOCATION_LOCAL, new LocalFileOperator());
        CONCRETE_OPT.put(STORAGE_LOCATION_ALI_OSS, new AliyunOssFileOperator());
    }

    private final IFileOperator currentOpt;

    public FileOperatorContext(Integer storageLocation) {
        this.currentOpt = CONCRETE_OPT.get(storageLocation);
    }

    public FileOperatorContext() {
        Integer storageLocation = SysConfigService.getIntByKey(AdiConstant.SysConfigKey.STORAGE_LOCATION, -1);
        this.currentOpt = CONCRETE_OPT.get(storageLocation);
    }

    public static boolean checkIfExist(AdiFile adiFile) {
        return CONCRETE_OPT.get(adiFile.getStorageLocation()).checkIfExist(adiFile);
    }

    public Pair<String, String> save(MultipartFile file, boolean image, String fileName) {
        return currentOpt.save(file, image, fileName);
    }

    public Pair<String, String> save(byte[] file, boolean image, String fileName) {
        return currentOpt.save(file, image, fileName);
    }

    public SaveRemoteImageResult saveImageFromUrl(String imageUrl, String uuid) {
        return currentOpt.saveImageFromUrl(imageUrl, uuid);
    }

    public static void delete(AdiFile adiFile) {
        CONCRETE_OPT.get(adiFile.getStorageLocation()).delete(adiFile);
    }

    public static String getFileUrl(AdiFile adiFile) {
        return CONCRETE_OPT.get(adiFile.getStorageLocation()).getFileUrl(adiFile);
    }

    public static Document loadDocument(AdiFile adiFile) {
        return CONCRETE_OPT.get(adiFile.getStorageLocation()).loadDocument(adiFile);
    }

    public static int getStorageLocation() {
        return SysConfigService.getIntByKey(AdiConstant.SysConfigKey.STORAGE_LOCATION, -1);
    }
}
