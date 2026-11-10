package com.moyz.adi.common.mapper;

import com.moyz.adi.common.dto.FileRefCountDto;
import com.moyz.adi.common.entity.AdiFile;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 无引用文件回收的数据面：引用实查（全扫描面 UNION）、候选/到期行游标分批、行状态流转。
 * 行状态流转绕开线程上下文（GC 定时任务无 ThreadContext），全部按主键定位。
 * <p>
 * Data plane of the unreferenced-file GC: reference recount across the full scan surface,
 * cursor-batched candidate/expired listing and row state transitions keyed by primary id
 * (the scheduled job runs without a ThreadContext).
 * </p>
 */
@Mapper
public interface FileGcMapper {

    List<FileRefCountDto> countRefs(@Param("uuids") List<String> uuids);

    List<AdiFile> listUnreferencedCandidates(@Param("lastId") Long lastId, @Param("ttl") LocalDateTime ttl, @Param("batchSize") int batchSize);

    List<AdiFile> listGraceExpired(@Param("lastId") Long lastId, @Param("before") LocalDateTime before, @Param("batchSize") int batchSize);

    int softDeleteById(@Param("id") Long id);

    int updateRefCount(@Param("id") Long id, @Param("refCount") int refCount);

    int reviveById(@Param("id") Long id, @Param("refCount") int refCount);

    int deleteById(@Param("id") Long id);
}
