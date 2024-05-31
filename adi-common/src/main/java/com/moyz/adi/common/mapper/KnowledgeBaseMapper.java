package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.entity.KnowledgeBase;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;

@Mapper
public interface KnowledgeBaseMapper extends BaseMapper<KnowledgeBase> {

    /**
     * 搜索知识库（管理员）
     *
     * @param keyword 关键词
     * @return
     */
    Page<KnowledgeBase> searchByAdmin(Page<KnowledgeBase> page, @Param("keyword") String keyword);

    /**
     * 搜索知识库（用户）
     *
     * @param ownerId 用户id
     * @param keyword 关键词
     * @return
     */
    Page<KnowledgeBase> searchByUser(Page<KnowledgeBase> page, @Param("ownerId") long ownerId, @Param("keyword") String keyword, @Param("includeOthersPublic") Boolean includeOthersPublic);

    /**
     * 更新统计数据
     *
     * @param uuid
     */
    void updateStatByUuid(@Param("uuid") String uuid);

    Integer countCreatedByTimePeriod(@Param("beginTime") LocalDateTime beginTime, @Param("endTime") LocalDateTime endTime);

    Integer countAllCreated();
}
