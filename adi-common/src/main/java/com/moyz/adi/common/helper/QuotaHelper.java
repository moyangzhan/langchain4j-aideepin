package com.moyz.adi.common.helper;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.vo.CostStat;
import com.moyz.adi.common.service.UserDayCostService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class QuotaHelper {

    @Resource
    private UserDayCostService userDayCostService;

    public ErrorEnum checkTextQuota(User user) {
        int userQuotaByTokenDay = user.getQuotaByTokenDaily();
        int userQuotaByTokenMonth = user.getQuotaByTokenMonthly();
        int userQuotaByRequestDay = user.getQuotaByRequestDaily();
        int userQuotaByRequestMonth = user.getQuotaByRequestMonthly();
        CostStat costStat = userDayCostService.costStatByUser(user.getId(), false);
        if (costStat.getTextTokenCostByDay() >= userQuotaByTokenDay || costStat.getTextRequestTimesByDay() >= userQuotaByRequestDay) {
            log.warn("Reach limit of a day,userId:{},token:{},request:{},used token:{}, used request:{}", user.getId(), userQuotaByRequestDay, userQuotaByRequestDay, userQuotaByTokenMonth, userQuotaByRequestMonth);
            return ErrorEnum.B_DAILY_QUOTA_USED;
        }
        if (costStat.getTextTokenCostByMonth() >= user.getQuotaByTokenMonthly() || costStat.getTextRequestTimesByMonth() >= user.getQuotaByRequestMonthly()) {
            log.warn("Reach limit of a month,userId:{},token:{},request:{},used token:{}, used request:{}", user.getId(), user.getQuotaByTokenMonthly(), user.getQuotaByRequestMonthly(), costStat.getTextTokenCostByMonth(), costStat.getTextRequestTimesByMonth());
            return ErrorEnum.B_MONTHLY_QUOTA_USED;
        }
        return null;
    }

    /**
     * Check the generate image request if it can be accepted
     *
     * @param user 要检查的用户
     * @return 错误码
     */
    public ErrorEnum checkImageQuota(User user, boolean isFree) {
        int userDailyQuota = user.getQuotaByImageDaily();
        int userMonthlyQuota = user.getQuotaByImageMonthly();
        CostStat costStat = userDayCostService.costStatByUser(user.getId(), isFree);
        if (costStat.getImageGeneratedNumberByDay() >= userDailyQuota) {
            log.warn("Generate image reach limit of a day,userId:{},request quota:{},used request times:{}", user.getId(), userDailyQuota, costStat.getImageGeneratedNumberByDay());
            return ErrorEnum.B_DAILY_QUOTA_USED;
        }
        if (costStat.getImageGeneratedNumberByMonth() >= userMonthlyQuota) {
            log.warn("Generate image reach limit of a month,userId:{},request quota:{},used request times:{}", user.getId(), user.getQuotaByImageMonthly(), costStat.getImageGeneratedNumberByMonth());
            return ErrorEnum.B_MONTHLY_QUOTA_USED;
        }
        return null;
    }
}
