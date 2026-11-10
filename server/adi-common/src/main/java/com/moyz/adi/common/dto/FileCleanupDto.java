package com.moyz.adi.common.dto;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class FileCleanupDto {

    private String uuid;

    private String name;

    private String ext;

    private Long size;

    /**
     * 文件访问路径：本地存储为 /file/{uuid}.{ext} 相对路径（前端拼 /api 前缀带 token 预览），OSS 为完整外链。
     * <p>
     * Access path: a relative /file/{uuid}.{ext} for local storage (the frontend prefixes /api and a
     * token to preview), or a full public link for OSS.
     * </p>
     */
    private String url;

    private Long userId;

    private String userName;

    private LocalDateTime createTime;
}
