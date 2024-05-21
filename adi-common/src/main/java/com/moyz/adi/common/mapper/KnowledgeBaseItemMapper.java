package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.annotation.InterceptorIgnore;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbItemDto;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface KnowledgeBaseItemMapper extends BaseMapper<KnowledgeBaseItem> {

    @InterceptorIgnore(tenantLine = "true")
    Page<KbItemDto> searchByKb(Page<KbItemDto> page, @Param("kbUuid") String kbUuid, @Param("keyword") String keyword);

    KnowledgeBaseItem getByUuid(String uuid);
}
