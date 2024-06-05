package com.moyz.adi.common.dto;

import com.moyz.adi.common.enums.UserStatusEnum;
import lombok.Data;

@Data
public class UserEditReq {

    private String uuid;

    private String name;

    private String password;

    private UserStatusEnum userStatus;

    private Integer quotaByTokenDaily;

    private Integer quotaByTokenMonthly;

    private Integer quotaByRequestDaily;

    private Integer quotaByRequestMonthly;

    private Integer quotaByImageDaily;

    private Integer quotaByImageMonthly;

    private Boolean isAdmin;
}
