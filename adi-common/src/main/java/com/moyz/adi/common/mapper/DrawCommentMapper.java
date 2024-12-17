package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.DrawCommentDto;
import com.moyz.adi.common.entity.DrawComment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface DrawCommentMapper extends BaseMapper<DrawComment> {
    Page<DrawCommentDto> listByPage(Page<DrawCommentDto> page, @Param("drawId") Long drawId);
}
