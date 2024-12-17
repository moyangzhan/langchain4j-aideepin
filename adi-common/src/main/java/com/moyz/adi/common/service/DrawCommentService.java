package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.moyz.adi.common.dto.DrawCommentDto;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.DrawComment;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.DrawCommentMapper;
import com.moyz.adi.common.util.PrivilegeUtil;
import com.moyz.adi.common.util.RedisTemplateUtil;
import com.moyz.adi.common.util.UuidUtil;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.text.MessageFormat;

import static com.moyz.adi.common.cosntant.RedisKeyConstant.DRAW_COMMENT_LIMIT_KEY;
import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.A_OPT_TOO_FREQUENTLY;

@Slf4j
@Service
public class DrawCommentService extends ServiceImpl<DrawCommentMapper, DrawComment> {

    @Resource
    private RedisTemplateUtil redisTemplateUtil;

    /**
     * 同一用户，5秒内只能提交一次评论
     *
     * @param user   用户
     * @param draw   绘图
     * @param remark 评论内容
     * @return 评论
     */
    public DrawCommentDto add(User user, Draw draw, String remark) {

        String redisKey = MessageFormat.format(DRAW_COMMENT_LIMIT_KEY, user.getId());
        if (!redisTemplateUtil.lock(redisKey, UuidUtil.createShort(), 5)) {
            throw new BaseException(A_OPT_TOO_FREQUENTLY);
        }
        String uuid = UuidUtil.createShort();
        DrawComment newObj = new DrawComment();
        newObj.setUuid(uuid);
        newObj.setUserId(draw.getUserId());
        newObj.setDrawId(draw.getId());
        newObj.setRemark(remark);
        baseMapper.insert(newObj);

        return DrawCommentDto.builder()
                .uuid(uuid)
                .drawUuid(draw.getUuid())
                .userUuid(user.getUuid())
                .userName(user.getName())
                .remark(remark)
                .createTime(newObj.getCreateTime())
                .build();
    }

    public Page<DrawCommentDto> listByPage(long drawId, int currentPage, int pageSize) {
        return baseMapper.listByPage(new Page<>(currentPage, pageSize), drawId);
    }

    public boolean softDel(Long id) {
        DrawComment drawComment = PrivilegeUtil.checkAndGetById(id, this.query(), A_DATA_NOT_FOUND);
        return this.lambdaUpdate().eq(DrawComment::getId, drawComment.getId()).set(DrawComment::getIsDeleted, true).update();
    }

}
