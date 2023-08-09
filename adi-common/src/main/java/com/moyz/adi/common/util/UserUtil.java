package com.moyz.adi.common.util;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.User;
import org.apache.commons.lang3.StringUtils;

public class UserUtil {

    public static int getSecretType(User user) {
        return StringUtils.isNotBlank(user.getSecretKey()) ? AdiConstant.SECRET_KEY_TYPE_CUSTOM : AdiConstant.SECRET_KEY_TYPE_SYSTEM;
    }
}
