package com.moyz.adi.common.helper;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.vo.CostStat;
import com.moyz.adi.common.service.SysConfigService;
import com.moyz.adi.common.service.UserDayCostService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class QuotaHelper {

    @Resource
    private UserDayCostService userDayCostService;

    public ErrorEnum checkTextQuota(User user) {
        int quotaByTokenDay = resolveQuota(user.getQuotaByTokenDaily(), AdiConstant.SysConfigKey.QUOTA_BY_TOKEN_DAILY);
        int quotaByTokenMonth = resolveQuota(user.getQuotaByTokenMonthly(), AdiConstant.SysConfigKey.QUOTA_BY_TOKEN_MONTHLY);
        int quotaByRequestDay = resolveQuota(user.getQuotaByRequestDaily(), AdiConstant.SysConfigKey.QUOTA_BY_REQUEST_DAILY);
        int quotaByRequestMonth = resolveQuota(user.getQuotaByRequestMonthly(), AdiConstant.SysConfigKey.QUOTA_BY_REQUEST_MONTHLY);

        CostStat costStat = userDayCostService.costStatByUser(user.getId(), false);

        boolean dailyTokenExceeded = quotaByTokenDay > 0 && costStat.getTextTokenCostByDay() >= quotaByTokenDay;
        boolean dailyRequestExceeded = quotaByRequestDay > 0 && costStat.getTextRequestTimesByDay() >= quotaByRequestDay;
        if (dailyTokenExceeded || dailyRequestExceeded) {
            log.warn("Reach limit of a day,userId:{},token quota:{},request quota:{},used token:{}, used request:{}", user.getId(), quotaByTokenDay, quotaByRequestDay, costStat.getTextTokenCostByDay(), costStat.getTextRequestTimesByDay());
            return ErrorEnum.B_DAILY_QUOTA_USED;
        }

        boolean monthlyTokenExceeded = quotaByTokenMonth > 0 && costStat.getTextTokenCostByMonth() >= quotaByTokenMonth;
        boolean monthlyRequestExceeded = quotaByRequestMonth > 0 && costStat.getTextRequestTimesByMonth() >= quotaByRequestMonth;
        if (monthlyTokenExceeded || monthlyRequestExceeded) {
            log.warn("Reach limit of a month,userId:{},token quota:{},request quota:{},used token:{}, used request:{}", user.getId(), quotaByTokenMonth, quotaByRequestMonth, costStat.getTextTokenCostByMonth(), costStat.getTextRequestTimesByMonth());
            return ErrorEnum.B_MONTHLY_QUOTA_USED;
        }

        return null;
    }

    public ErrorEnum checkImageQuota(User user, boolean isFree) {
        int dailyQuota = resolveQuota(user.getQuotaByImageDaily(), AdiConstant.SysConfigKey.QUOTA_BY_IMAGE_DAILY);
        int monthlyQuota = resolveQuota(user.getQuotaByImageMonthly(), AdiConstant.SysConfigKey.QUOTA_BY_IMAGE_MONTHLY);

        CostStat costStat = userDayCostService.costStatByUser(user.getId(), isFree);

        if (dailyQuota > 0 && costStat.getDrawTimesByDay() >= dailyQuota) {
            log.warn("Generate image reach limit of a day,userId:{},request quota:{},used request times:{}", user.getId(), dailyQuota, costStat.getDrawTimesByDay());
            return ErrorEnum.B_DAILY_QUOTA_USED;
        }
        if (monthlyQuota > 0 && costStat.getDrawTimesByMonth() >= monthlyQuota) {
            log.warn("Generate image reach limit of a month,userId:{},request quota:{},used request times:{}", user.getId(), monthlyQuota, costStat.getDrawTimesByMonth());
            return ErrorEnum.B_MONTHLY_QUOTA_USED;
        }

        return null;
    }

    /**
     * Resolve quota: use user-specific value if set (>0), otherwise fall back to system default.
     */
    private int resolveQuota(int userQuota, String sysConfigKey) {
        if (userQuota > 0) {
            return userQuota;
        }
        return SysConfigService.getIntByKey(sysConfigKey, 0);
    }
}
