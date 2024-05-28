package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.UserStatusEnum;
import lombok.Data;

@Data
public class UsersReq {
    String name;
    String email;
    String uuid;

    UserStatusEnum userStatus;

    Boolean isAdmin;
}
