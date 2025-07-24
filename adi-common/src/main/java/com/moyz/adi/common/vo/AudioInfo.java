package com.moyz.adi.common.vo;

import lombok.Data;

@Data
public class AudioInfo {
    /**
     * 响应内容对应的音频文件路径，本地绝对路径或OSS url
     */
    private String path;

    /**
     * 已经保存到数据库的音频文件的UUID
     */
    private String uuid;

    /**
     * 本系统访问音频文件的URL，通常是一个HTTP或HTTPS链接（注意与oss的链接区分开）
     */
    private String url;

    /**
     * 音频文件的时长，单位为秒
     */
    private Integer duration;
}
