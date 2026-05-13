package com.moyz.adi.common.util;

import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.web.multipart.MultipartFile;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

@Slf4j
public class HashUtil {

    private HashUtil() {
    }

    public static String sha256(MultipartFile file) {
        try {
            MessageDigest sha256 = MessageDigest.getInstance("SHA-256");
            byte[] hash = sha256.digest(file.getBytes());
            // 将哈希值转换为十六进制字符串
            return getHashStr(hash);
        } catch (NoSuchAlgorithmException e) {
            log.error("Calculate file sha256 NoSuchAlgorithmException", e);
            throw new BaseException(ErrorEnum.B_SERVER_EXCEPTION);
        } catch (Exception e) {
            log.error("Calculate file sha256 error", e);
            throw new BaseException(ErrorEnum.B_SERVER_EXCEPTION);
        }
    }

    public static String sha256(String str) {
        try {
            MessageDigest sha256 = MessageDigest.getInstance("SHA-256");
            byte[] hash = sha256.digest(str.getBytes(StandardCharsets.UTF_8));
            // 将哈希值转换为十六进制字符串
            return getHashStr(hash);
        } catch (NoSuchAlgorithmException e) {
            log.error("Calculate string sha256 NoSuchAlgorithmException", e);
            throw new BaseException(ErrorEnum.B_SERVER_EXCEPTION);
        } catch (Exception e) {
            log.error("Calculate string sha256 error", e);
            throw new BaseException(ErrorEnum.B_SERVER_EXCEPTION);
        }
    }

    @NotNull
    private static String getHashStr(byte[] hash) {
        StringBuilder hexString = new StringBuilder();
        for (byte b : hash) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
}
