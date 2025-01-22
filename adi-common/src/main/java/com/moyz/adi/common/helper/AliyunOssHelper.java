package com.moyz.adi.common.helper;

import com.aliyun.core.utils.StringUtils;
import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.model.DeleteObjectsRequest;
import com.aliyun.oss.model.DeleteObjectsResult;
import com.aliyun.oss.model.PutObjectResult;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.vo.AliOssConfig;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

@Slf4j
@Service
public class AliyunOssHelper {

    private OSS client = null;
    @Getter
    private AliOssConfig configObj = null;
    private String configStr = "";

    public void init() {
        String newConfigStr = LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.STORAGE_LOCATION_ALI_OSS);
        if (StringUtils.isBlank(newConfigStr)) {
            throw new BaseException(ErrorEnum.C_ALI_OSS_CONFIG_ERROR, "阿里云OSS配置异常：数据表adi_system_config中找不到对应的配置行file_store_location_ali_oss");
        }
        if (configStr.equals(newConfigStr)) {
            return;
        }
        //配置有变化，重新加载
        AliOssConfig newConfigObj = JsonUtil.fromJson(newConfigStr, AliOssConfig.class);
        if (null == newConfigObj) {
            throw new BaseException(ErrorEnum.C_ALI_OSS_CONFIG_ERROR, "阿里云OSS配置异常：没有正确填写配置项file_store_location_ali_oss的内容");
        }
        if (null != client) {
            client.shutdown();
        }
        configStr = newConfigStr;
        configObj = newConfigObj;
        client = new OSSClientBuilder().build(configObj.getEndpoint(), configObj.getAccessKeyId(), configObj.getAccessKeySecret());
    }

    public void saveObj(byte[] bytes, String name) {
        InputStream is = new ByteArrayInputStream(bytes);
        PutObjectResult putObjectResult = client.putObject(configObj.getBucketName(), name, is);
        if (null != putObjectResult) {
            log.info("Ali oss put object:{}", putObjectResult.getETag());
        }
    }

    public void deleteObjs(List<String> objectNames) {
        DeleteObjectsResult deleteObjectsResult = client.deleteObjects(new DeleteObjectsRequest(configObj.getBucketName()).withKeys(objectNames));
        List<String> deletedObjects = deleteObjectsResult.getDeletedObjects();
        for (String object : deletedObjects) {
            log.warn("Object {} deleted", object);
        }
    }

    public boolean doesObjectExist(String objectName) {
        return client.doesObjectExist(configObj.getBucketName(), objectName);
    }

    /**
     * 获取完整访问路径 | Get the full access URL
     *
     * @param objectName 对象名称 | Object name
     * @return 完整访问路径 | Full access URL
     */
    public String getUrl(String objectName) {
        return "https://" + configObj.getBucketName() + "." + configObj.getEndpoint() + "/" + objectName;
    }
}
