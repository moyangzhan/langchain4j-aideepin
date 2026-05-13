package com.moyz.adi.common.service;

import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.UserStatusEnum;
import com.moyz.adi.common.vo.*;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.*;

@Slf4j
@Service
public class StatisticService {

    @Resource
    private UserService userService;

    @Resource
    private UserDayCostService userDayCostService;

    @Resource
    private KnowledgeBaseService knowledgeBaseService;

    @Resource
    private KnowledgeBaseItemService knowledgeBaseItemService;

    @Resource
    private ConversationService conversationService;

    @Resource
    private DrawService drawService;

    @Cacheable(value = STATISTIC + ":" + STATISTIC_USER)
    public UserStatistic calUserStat() {
        UserStatistic result = new UserStatistic();
        LocalDate today = LocalDate.now();
        int todayCreated = userService.lambdaQuery()
                .gt(User::getCreateTime, today)
                .count()
                .intValue();
        result.setTodayCreated(todayCreated);

        int todayActivated = userService.lambdaQuery()
                .gt(User::getCreateTime, today)
                .eq(User::getUserStatus, UserStatusEnum.NORMAL)
                .count()
                .intValue();
        int totalNormal = userService.lambdaQuery()
                .eq(User::getUserStatus, UserStatusEnum.NORMAL)
                .count()
                .intValue();

        result.setTodayCreated(todayCreated);
        result.setTotalNormal(totalNormal);
        result.setTodayActivated(todayActivated);
        return result;
    }

    @Cacheable(value = STATISTIC + ":" + STATISTIC_TOKEN_COST)
    public TokenCostStatistic calTokenCostStat() {
        Integer todayCost = userDayCostService.sumTodayCost();
        Integer currentMonthCost = userDayCostService.sumCurrentMonthCost();
        TokenCostStatistic aiModelStat = new TokenCostStatistic();
        aiModelStat.setTodayTokenCost(todayCost);
        aiModelStat.setMonthTokenCost(currentMonthCost);
        return aiModelStat;
    }

    @Cacheable(value = STATISTIC + ":" + STATISTIC_IMAGE_COST)
    public ImageCostStatistic calImageCostStat() {
        return ImageCostStatistic.builder()
                .todayCost(drawService.sumTodayCost())
                .monthCost(drawService.sumCurrMonthCost())
                .build();
    }

    @Cacheable(value = STATISTIC + ":" + STATISTIC_KNOWLEDGE_BASE)
    public KbStatistic calKbStat() {
        int kbTodayCreated = knowledgeBaseService.countTodayCreated();
        int kbTotal = knowledgeBaseService.countAllCreated();
        int itemTodayCreated = knowledgeBaseItemService.countTodayCreated();
        int itemTotal = knowledgeBaseItemService.countAllCreated();
        KbStatistic stat = new KbStatistic();
        stat.setKbTodayCreated(kbTodayCreated);
        stat.setKbTotal(kbTotal);
        stat.setItemTotal(itemTotal);
        stat.setItemTodayCreated(itemTodayCreated);
        return stat;
    }

    /**
     * 统计会话信息
     *
     * @return
     */
    @Cacheable(value = STATISTIC + ":" + STATISTIC_CONVERSATION)
    public ConvStatistic calConvStatistic() {
        return ConvStatistic.builder()
                .todayCreated(conversationService.countTodayCreated())
                .total(conversationService.countAllCreated())
                .build();
    }

}
