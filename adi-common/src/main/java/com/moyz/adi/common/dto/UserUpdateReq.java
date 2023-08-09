package com.moyz.adi.common.dto;

import com.moyz.adi.common.annotation.NotAllFieldsEmptyCheck;
import lombok.Data;

@Data
@NotAllFieldsEmptyCheck
public class UserUpdateReq {
    private String secretKey;
    private Boolean contextEnable;
    private Integer contextMsgPairNum;
}
