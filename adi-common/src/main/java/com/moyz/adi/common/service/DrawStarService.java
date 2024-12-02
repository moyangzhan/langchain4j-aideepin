package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.DrawStar;
import com.moyz.adi.common.mapper.DrawStarMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.List;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.USER_INFO;

@Slf4j
@Service
public class DrawStarService extends ServiceImpl<DrawStarMapper, DrawStar> {

    public List<DrawStar> listByCurrentUser(Long maxId, int pageSize) {
        return this.lambdaQuery()
                .eq(DrawStar::getUserId, ThreadContext.getCurrentUserId())
                .eq(DrawStar::getIsDeleted, false)
                .lt(DrawStar::getId, maxId)
                .orderByDesc(DrawStar::getId)
                .last("limit " + pageSize)
                .list();
    }

    @CacheEvict(cacheNames = USER_INFO, condition = "#drawId>0 && #userId>0", key = "'star:'+#drawId+':'+#userId")
    public boolean toggle(Long drawId, Long userId) {
        DrawStar drawStar = this.lambdaQuery()
                .eq(DrawStar::getDrawId, drawId)
                .eq(DrawStar::getUserId, userId)
                .one();
        if (null != drawStar) {
            return this.lambdaUpdate()
                    .set(DrawStar::getIsDeleted, !drawStar.getIsDeleted())
                    .eq(DrawStar::getId, drawStar.getId())
                    .update();
        } else {
            DrawStar newObj = new DrawStar();
            newObj.setDrawId(drawId);
            newObj.setUserId(userId);
            baseMapper.insert(newObj);
            return true;
        }
    }

    @Cacheable(cacheNames = USER_INFO, condition = "#drawId>0 && #userId>0", key = "'star:'+#drawId+':'+#userId")
    public boolean isStarred(Long drawId, Long userId) {
        DrawStar drawStar = this.lambdaQuery()
                .eq(DrawStar::getDrawId, drawId)
                .eq(DrawStar::getUserId, userId)
                .one();
        return null != drawStar && !drawStar.getIsDeleted();
    }

}
