package com.moyz.adi.common.service;

import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.entity.UserDayCost;
import com.moyz.adi.common.mapper.UserDayCostMapper;
import com.moyz.adi.common.vo.CostStat;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.util.UserUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Service
public class UserDayCostService extends ServiceImpl<UserDayCostMapper, UserDayCost> {

    public void appendCostToUser(User user, int tokens) {
        UserDayCost userDayCost = getTodayCost(user);
        UserDayCost saveOrUpdateInst = new UserDayCost();
        if (null == userDayCost) {
            saveOrUpdateInst.setUserId(user.getId());
            saveOrUpdateInst.setDay(LocalDateTimeUtil.getToday());
            saveOrUpdateInst.setTokens(tokens);
            saveOrUpdateInst.setRequests(1);
            saveOrUpdateInst.setSecretKeyType(UserUtil.getSecretType(user));
        } else {
            saveOrUpdateInst.setId(userDayCost.getId());
            saveOrUpdateInst.setTokens(userDayCost.getTokens() + tokens);
            saveOrUpdateInst.setRequests(userDayCost.getRequests() + 1);
        }
        saveOrUpdate(saveOrUpdateInst);
    }

    public CostStat costStatByUser(long userId) {
        CostStat result = new CostStat();

        int today = LocalDateTimeUtil.getIntDay(LocalDateTime.now());
        int start = LocalDateTimeUtil.getIntDay(LocalDateTime.now().withDayOfMonth(1));
        int end = LocalDateTimeUtil.getIntDay(LocalDateTime.now().plusMonths(1).withDayOfMonth(1).minusDays(1));

        List<UserDayCost> userDayCostList = this.lambdaQuery()
                .eq(UserDayCost::getUserId, userId)
                .eq(UserDayCost::getSecretKeyType, AdiConstant.SECRET_KEY_TYPE_SYSTEM)
                .between(UserDayCost::getDay, start, end)
                .list();
        for (UserDayCost userDayCost : userDayCostList) {
            result.setTextTokenCostByMonth(result.getTextTokenCostByMonth() + userDayCost.getTokens());
            result.setTextRequestTimesByMonth(result.getTextRequestTimesByMonth() + userDayCost.getRequests());
            result.setImageGeneratedNumberByMonth(result.getImageGeneratedNumberByMonth() + userDayCost.getImagesNumber());
            if (userDayCost.getDay() == today) {
                result.setTextTokenCostByDay(userDayCost.getTokens());
                result.setTextRequestTimesByDay(userDayCost.getRequests());
                result.setImageGeneratedNumberByDay(userDayCost.getImagesNumber());
            }
        }
        return result;
    }

    public UserDayCost getTodayCost(User user) {
        return this.lambdaQuery()
                .eq(UserDayCost::getUserId, user.getId())
                .eq(UserDayCost::getDay, LocalDateTimeUtil.getToday())
                .eq(UserDayCost::getSecretKeyType, UserUtil.getSecretType(user))
                .one();
    }
}
