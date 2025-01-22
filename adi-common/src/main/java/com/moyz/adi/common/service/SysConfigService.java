package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.SysConfigDto;
import com.moyz.adi.common.dto.SysConfigEditDto;
import com.moyz.adi.common.dto.SysConfigSearchReq;
import com.moyz.adi.common.entity.SysConfig;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.SysConfigMapper;
import com.moyz.adi.common.util.JsonUtil;
import com.moyz.adi.common.util.LocalCache;
import com.moyz.adi.common.util.MPPageUtil;
import com.moyz.adi.common.vo.RequestRateLimit;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
public class SysConfigService extends ServiceImpl<SysConfigMapper, SysConfig> {

    public void loadAndCache() {
        List<SysConfig> configsFromDB = this.lambdaQuery().eq(SysConfig::getIsDeleted, false).list();
        if (LocalCache.CONFIGS.isEmpty()) {
            configsFromDB.forEach(item -> LocalCache.CONFIGS.put(item.getName(), item.getValue()));
        } else {
            //remove deleted config
            List<String> deletedKeys = new ArrayList<>();
            LocalCache.CONFIGS.forEach((k, v) -> {
                boolean deleted = configsFromDB.stream().noneMatch(sysConfig -> sysConfig.getName().equals(k));
                if (deleted) {
                    deletedKeys.add(k);
                }
            });
            if (!deletedKeys.isEmpty()) {
                deletedKeys.forEach(LocalCache.CONFIGS::remove);
            }

            //add or update config
            for (SysConfig item : configsFromDB) {
                String key = item.getName();
                LocalCache.CONFIGS.put(key, item.getValue());
            }
        }
        LocalCache.TEXT_RATE_LIMIT_CONFIG = JsonUtil.fromJson(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.REQUEST_TEXT_RATE_LIMIT), RequestRateLimit.class);
        LocalCache.IMAGE_RATE_LIMIT_CONFIG = JsonUtil.fromJson(LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.REQUEST_IMAGE_RATE_LIMIT), RequestRateLimit.class);
        LocalCache.TEXT_RATE_LIMIT_CONFIG.setType(RequestRateLimit.TYPE_TEXT);
        LocalCache.IMAGE_RATE_LIMIT_CONFIG.setType(RequestRateLimit.TYPE_IMAGE);
    }

    public void edit(SysConfigEditDto sysConfigDto) {
        LambdaQueryWrapper<SysConfig> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(SysConfig::getName, sysConfigDto.getName());
        lambdaQueryWrapper.eq(SysConfig::getIsDeleted, false);
        SysConfig existOne = baseMapper.selectOne(lambdaQueryWrapper);
        if (null == existOne) {
            throw new BaseException(ErrorEnum.A_DATA_NOT_FOUND);
        }
        SysConfig updateOne = new SysConfig();
        updateOne.setId(existOne.getId());
        updateOne.setValue(sysConfigDto.getValue());
        baseMapper.updateById(updateOne);

        loadAndCache();
    }

    public void softDelete(Long id) {
        SysConfig sysConfig = new SysConfig();
        sysConfig.setIsDeleted(true);
        sysConfig.setId(id);
        baseMapper.updateById(sysConfig);

        loadAndCache();
    }

    public int getConversationMaxNum() {
        String maxNum = LocalCache.CONFIGS.get(AdiConstant.SysConfigKey.CONVERSATION_MAX_NUM);
        return Integer.parseInt(maxNum);
    }

    public static String getByKey(String key) {
        return LocalCache.CONFIGS.get(key);
    }

    public static Integer getIntByKey(String key, int defaultValue) {
        String val = LocalCache.CONFIGS.get(key);
        if (null != val) {
            return Integer.parseInt(val);
        }
        return defaultValue;
    }


    public Page<SysConfigDto> search(SysConfigSearchReq searchReq, Integer currentPage, Integer pageSize) {
        LambdaQueryWrapper<SysConfig> wrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(searchReq.getKeyword())) {
            wrapper.like(SysConfig::getName, searchReq.getKeyword());
        }
        if (CollectionUtils.isNotEmpty(searchReq.getNames())) {
            wrapper.in(SysConfig::getName, searchReq.getNames());
        }
        wrapper.eq(SysConfig::getIsDeleted, false);
        Page<SysConfig> page = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);
        Page<SysConfigDto> result = new Page<>();
        return MPPageUtil.convertToPage(page, result, SysConfigDto.class);
    }

}
