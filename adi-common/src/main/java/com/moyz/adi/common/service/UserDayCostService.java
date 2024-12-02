package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.UserDayCost;
import com.moyz.adi.common.mapper.UserDayCostMapper;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.vo.CostStat;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Service
public class UserDayCostService extends ServiceImpl<UserDayCostMapper, UserDayCost> {

    @Lazy
    @Resource
    private UserDayCostService self;

    /**
     * Append token cost
     *
     * @param user   用户
     * @param tokens The number of tokens
     * @param isFree 消耗的是否免费额度
     */
    public void appendCostToUser(User user, int tokens, boolean isFree) {
        log.info("用户{}增加消耗token数量:{}", user.getName(), tokens);
        if (tokens <= 0) {
            return;
        }
        UserDayCost userDayCost = getTodayCost(user, isFree);
        UserDayCost saveOrUpdateInst = new UserDayCost();
        if (null == userDayCost) {
            saveOrUpdateInst.setUserId(user.getId());
            saveOrUpdateInst.setDay(LocalDateTimeUtil.getToday());
            saveOrUpdateInst.setTokens(tokens);
            saveOrUpdateInst.setRequests(1);
        } else {
            saveOrUpdateInst.setId(userDayCost.getId());
            saveOrUpdateInst.setTokens(userDayCost.getTokens() + tokens);
            saveOrUpdateInst.setRequests(userDayCost.getRequests() + 1);
        }
        saveOrUpdateInst.setIsFree(isFree);
        self.saveOrUpdate(saveOrUpdateInst);
    }

    public CostStat costStatByUser(long userId, boolean isFree) {
        CostStat result = new CostStat();

        int today = LocalDateTimeUtil.getIntDay(LocalDateTime.now());
        int start = LocalDateTimeUtil.getIntDay(LocalDateTime.now().withDayOfMonth(1));
        int end = LocalDateTimeUtil.getIntDay(LocalDateTime.now().plusMonths(1).withDayOfMonth(1).minusDays(1));

        List<UserDayCost> userDayCostList = this.lambdaQuery()
                .eq(UserDayCost::getUserId, userId)
                .between(UserDayCost::getDay, start, end)
                .eq(UserDayCost::getIsFree, isFree)
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
            result.setFree(userDayCost.getIsFree());
        }
        return result;
    }

    public UserDayCost getTodayCost(User user, boolean isFree) {
        return this.lambdaQuery()
                .eq(UserDayCost::getUserId, user.getId())
                .eq(UserDayCost::getDay, LocalDateTimeUtil.getToday())
                .eq(UserDayCost::getIsFree, isFree)
                .one();
    }

    public Integer sumTodayCost() {
        int today = LocalDateTimeUtil.getToday();
        return baseMapper.sumCostByDay(today).intValue();
    }

    public Integer sumCurrentMonthCost() {
        int start = LocalDateTimeUtil.getIntDay(LocalDateTime.now().withDayOfMonth(1));
        int end = LocalDateTimeUtil.getIntDay(LocalDateTime.now().plusMonths(1).withDayOfMonth(1).minusDays(1));
        return baseMapper.sumCostByDayPeriod(start, end).intValue();
    }
}
