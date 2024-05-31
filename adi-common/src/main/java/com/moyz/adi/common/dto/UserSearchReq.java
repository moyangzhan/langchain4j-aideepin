package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.UserStatusEnum;
import lombok.Data;

@Data
public class UserSearchReq {
    private String name;
    private String email;
    private String uuid;
    private Integer userStatus;
    private Boolean isAdmin;
    private Long[] createTime;
    private Long[] updateTime;
}
