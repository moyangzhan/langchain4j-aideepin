package com.moyz.adi.common.dto;

import lombok.Data;

/**
 * 文件清理列表搜索条件：文件名与用户名均为模糊匹配，空即不过滤。
 * Search criteria for the file-cleanup list: file name and user name are both fuzzy; blank means no filter.
 */
@Data
public class FileSearchReq {

    private String name;

    private String userName;
}
