package com.moyz.adi.common.mapper;

import com.baomidou.mybatisplus.annotation.InterceptorIgnore;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.dto.KbItemDto;
import com.moyz.adi.common.entity.KnowledgeBaseItem;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;

@Mapper
public interface KnowledgeBaseItemMapper extends BaseMapper<KnowledgeBaseItem> {

    @InterceptorIgnore(tenantLine = "true")
    Page<KbItemDto> searchByKb(Page<KbItemDto> page, @Param("kbUuid") String kbUuid, @Param("keyword") String keyword);

    KnowledgeBaseItem getByUuid(String uuid);

    Integer countCreatedByTimePeriod(@Param("beginTime") LocalDateTime beginTime, @Param("endTime") LocalDateTime endTime);

    Integer countAllCreated();

    /**
     * Write-privilege check: the owner of the knowledge base that owns item {uuid}
     * must be {userId}. Admin bypass is handled in the Java layer; this query only
     * checks ownership.
     *
     * @param uuid   knowledge-base item uuid
     * @param userId current user id
     * @return >0 means the write is allowed
     */
    Integer checkWritePrivilege(@Param("uuid") String uuid, @Param("userId") Long userId);

    /**
     * Read-privilege check: the owner of the knowledge base that owns item {uuid}
     * is {userId}, or that knowledge base is public. Admin bypass is handled in
     * the Java layer; this query only checks ownership / public flag.
     *
     * @param uuid   knowledge-base item uuid
     * @param userId current user id
     * @return >0 means the read is allowed
     */
    Integer checkReadPrivilege(@Param("uuid") String uuid, @Param("userId") Long userId);

    /**
     * Write-privilege check keyed by knowledge-base uuid: the owner of knowledge
     * base {kbUuid} must be {userId}. Used when authorizing creation of a new item
     * (no item uuid exists yet). Admin bypass is handled in the Java layer.
     *
     * @param kbUuid knowledge-base uuid
     * @param userId current user id
     * @return >0 means the write is allowed
     */
    Integer checkWritePrivilegeByKb(@Param("kbUuid") String kbUuid, @Param("userId") Long userId);

    /**
     * Write-privilege check keyed by item id: the owner of the knowledge base
     * that owns item {id} must be {userId}. Used when authorizing an update, where
     * the client-controlled uuid cannot be trusted and the real target is resolved
     * by id. Admin bypass is handled in the Java layer.
     *
     * @param id     knowledge-base item id
     * @param userId current user id
     * @return >0 means the write is allowed
     */
    Integer checkWritePrivilegeById(@Param("id") Long id, @Param("userId") Long userId);
}
