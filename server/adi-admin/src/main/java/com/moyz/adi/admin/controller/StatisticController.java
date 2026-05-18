package com.moyz.adi.admin.controller;

import com.moyz.adi.common.dto.StatisticDto;
import com.moyz.adi.common.service.StatisticService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "统计数据 | Statistics", description = "统计数据 | Statistics")
@RestController
@RequestMapping("/admin/statistic")
@Validated
public class StatisticController {

    @Resource
    private StatisticService statisticService;

    @Operation(summary = "获取统计数据 | Get Statistics")
    @GetMapping("/info")
    public StatisticDto statistic() {
        StatisticDto result = new StatisticDto();
        result.setKbStatistic(statisticService.calKbStat());
        result.setUserStatistic(statisticService.calUserStat());
        result.setTokenCostStatistic(statisticService.calTokenCostStat());
        result.setCharacterStatistic(statisticService.calCharacterStatistic());
        result.setImageCostStatistic(statisticService.calImageCostStat());
        return result;
    }
}
