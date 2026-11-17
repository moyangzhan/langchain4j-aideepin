package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.FileCleanupDto;
import com.moyz.adi.common.dto.FileRefCountDto;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.mapper.FileGcMapper;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.moyz.adi.common.cosntant.AdiConstant.STORAGE_LOCATION_VALUE_LOCAL;

/**
 * 无引用文件回收：两步判定（ref_count=0 快筛 → 业务表实查复核）+ 两段式删除（软删保留 → 满月物理删除文件及对应的数据行）。
 * 实查>0 一律回写 ref_count（自愈假零漂移，最危险方向）；每轮运行开始直查数据库读开关，missing key 即停。
 * 同时承载管理端手动清理的判定核心：列表实时计算，删除前再次复核。
 * <p>
 * Unreferenced-file GC: two-step detection (ref_count=0 prescreen → live recount across business tables) plus
 * two-stage deletion (soft-delete retention → delete the physical file + hard-delete the row after one month). Any positive recount
 * writes ref_count back (self-heals the dangerous false-zero drift); the switch is read straight from the
 * database at the start of every round, a missing key stops the job. The same judgment core backs the admin
 * manual-cleanup menu: the list is computed live and every delete re-verifies references first.
 * </p>
 */
@Slf4j
@Service
public class FileGcService {

    /**
     * TTL 保护期（天）：两段式上传「已传未提交」窗口内的文件不进候选。
     * Guard window in days: files still inside the upload-then-submit window never become candidates.
     */
    private static final int CANDIDATE_TTL_DAYS = 7;

    /**
     * 软删保留期（月）：机器判定的可逆后悔窗口，期满物理删除文件及对应的数据行。
     * Retention period in months for soft-deleted rows; on expiry the physical file is deleted and its row hard-deleted.
     */
    private static final int RETENTION_MONTHS = 1;

    /**
     * 分批大小：候选复核、到期清运与手动列表实查的批粒度。
     * Batch size shared by candidate verification, retention-expiry cleanup and the manual list recount.
     */
    private static final int BATCH_SIZE = 200;

    @Resource
    private FileGcMapper fileGcMapper;

    @Resource
    private FileService fileService;

    @Resource
    private SysConfigService sysConfigService;

    @Resource
    private UserService userService;

    public boolean gcEnabled() {
        return Boolean.parseBoolean(sysConfigService.getRawByKey(AdiConstant.SysConfigKey.FILE_GC_ENABLED));
    }

    public void gc() {
        if (!gcEnabled()) {
            return;
        }
        markUnreferenced();
        hardDeleteExpired();
    }

    /**
     * 阶段一：候选判定与软删。
     * 1. 游标分批取 ref_count=0 且过保护期的候选
     * 2. 逐批重算活跃引用
     * 3. 重算=0 软删行进保留期；>0 回写计数
     * <p>
     * Phase one: candidate detection and soft delete.
     * 1. cursor-batch candidates with ref_count=0 past the guard window
     * 2. recount the live references per batch
     * 3. zero recount soft-deletes the row into retention; positive writes back
     * </p>
     */
    void markUnreferenced() {
        LocalDateTime ttl = LocalDateTime.now().minusDays(CANDIDATE_TTL_DAYS);
        long lastId = 0L;
        int marked = 0;
        int healed = 0;
        while (true) {
            List<AdiFile> candidates = fileGcMapper.listUnreferencedCandidates(lastId, ttl, BATCH_SIZE);
            if (candidates.isEmpty()) {
                break;
            }
            lastId = candidates.get(candidates.size() - 1).getId();
            Map<String, Long> refs = countRefs(candidates.stream().map(AdiFile::getUuid).toList());
            for (AdiFile candidate : candidates) {
                long total = refs.getOrDefault(candidate.getUuid(), 0L);
                if (total > 0) {
                    fileGcMapper.updateRefCount(candidate.getId(), (int) total);
                    healed++;
                } else {
                    fileGcMapper.softDeleteById(candidate.getId());
                    marked++;
                }
            }
        }
        if (marked > 0 || healed > 0) {
            log.info("File GC phase one finished, soft-deleted:{}, ref-count healed:{}", marked, healed);
        }
    }

    /**
     * 阶段二：保留期结束后执行清运操作。
     * 1. 游标分批取软删满保留期的行
     * 2. 逐批重算活跃引用
     * 3. 每行删除前复核后处置
     * 处置规则：重算=0 先记录行快照日志再硬删行，path 独占时一并物理删字节；
     * 重算>0 且物理文件仍在则复活并回写；文件已不在保持软删等待。
     * <p>
     * Phase two: hard-delete retention-expired rows.
     * 1. cursor-batch soft-deleted rows past the retention period
     * 2. recount the live references per batch
     * 3. re-verify each row before disposing of it
     * Disposition: zero recount logs the row snapshot before hard-deleting the row, deleting the
     * bytes only when the path is unshared; positive recount revives the row when the file
     * still exists, otherwise it stays soft-deleted waiting.
     * </p>
     */
    void hardDeleteExpired() {
        LocalDateTime before = LocalDateTime.now().minusMonths(RETENTION_MONTHS);
        long lastId = 0L;
        int hardDeleted = 0;
        int revived = 0;
        while (true) {
            List<AdiFile> expired = fileGcMapper.listRetentionExpired(lastId, before, BATCH_SIZE);
            if (expired.isEmpty()) {
                break;
            }
            lastId = expired.get(expired.size() - 1).getId();
            Map<String, Long> refs = countRefs(expired.stream().map(AdiFile::getUuid).toList());
            for (AdiFile adiFile : expired) {
                long total = refs.getOrDefault(adiFile.getUuid(), 0L);
                if (total == 0) {
                    boolean shared = pathSharedByOtherRow(adiFile);
                    if (shared || deletePhysicalFile(adiFile)) {
                        log.info("Hard delete file row, id:{}, uuid:{}, userId:{}, name:{}, ext:{}, sha256:{}, storageLocation:{}, path:{}, createTime:{}, bytes:{}",
                                adiFile.getId(), adiFile.getUuid(), adiFile.getUserId(), adiFile.getName(), adiFile.getExt(),
                                adiFile.getSha256(), adiFile.getStorageLocation(), adiFile.getPath(), adiFile.getCreateTime(),
                                shared ? "kept" : "deleted");
                        fileGcMapper.deleteById(adiFile.getId());
                        hardDeleted++;
                    }
                } else if (physicalFileExists(adiFile)) {
                    fileGcMapper.reviveById(adiFile.getId(), (int) total);
                    revived++;
                    log.info("File revived inside retention window, uuid:{}, refs:{}", adiFile.getUuid(), total);
                } else {
                    log.warn("Soft-deleted file still referenced but physical file is gone, keep waiting, uuid:{}, refs:{}",
                            adiFile.getUuid(), total);
                }
            }
        }
        if (hardDeleted > 0 || revived > 0) {
            log.info("File GC phase two finished, hard-deleted:{}, revived:{}", hardDeleted, revived);
        }
    }

    /**
     * 管理端手动清理列表。
     * 1. 按快筛条件分页计数
     * 2. 分批取候选并逐批重算
     * 3. 有引用的剔除并回写，无引用的凑满当前页
     * total 为快筛计数；支持文件名与用户名模糊过滤。
     * <p>
     * Admin manual-cleanup list.
     * 1. count pages by the quick-screen predicate
     * 2. fetch candidate batches and recount each batch
     * 3. referenced rows are skipped with counts written back, unreferenced fill the page
     * The total stays the quick-screen count; fuzzy file-name and user-name filters supported.
     * </p>
     */
    public Page<FileCleanupDto> search(Integer currentPage, Integer pageSize, String name, String userName) {
        LocalDateTime ttl = LocalDateTime.now().minusDays(CANDIDATE_TTL_DAYS);
        List<Long> userIds = userIdsByName(userName);
        if (null != userIds && userIds.isEmpty()) {
            Page<FileCleanupDto> empty = new Page<>(currentPage, pageSize, 0);
            empty.setRecords(List.of());
            return empty;
        }
        Long total = fileService.count(candidatesWrapper(ttl, name, userIds, null, false));
        List<FileCleanupDto> confirmed = new ArrayList<>(pageSize);
        int toSkip = (currentPage - 1) * pageSize;
        Long lastId = null;
        while (confirmed.size() < pageSize) {
            List<AdiFile> candidates = fileService.page(Page.of(1, BATCH_SIZE, false), candidatesWrapper(ttl, name, userIds, lastId, true))
                    .getRecords();
            if (candidates.isEmpty()) {
                break;
            }
            lastId = candidates.get(candidates.size() - 1).getId();
            Map<String, Long> refs = countRefs(candidates.stream().map(AdiFile::getUuid).toList());
            Map<Long, String> userNames = userNames(candidates);
            for (AdiFile candidate : candidates) {
                long refTotal = refs.getOrDefault(candidate.getUuid(), 0L);
                if (refTotal > 0) {
                    fileGcMapper.updateRefCount(candidate.getId(), (int) refTotal);
                } else if (toSkip > 0) {
                    toSkip--;
                } else {
                    confirmed.add(toDto(candidate, userNames));
                    if (confirmed.size() == pageSize) {
                        break;
                    }
                }
            }
        }
        Page<FileCleanupDto> result = new Page<>(currentPage, pageSize, total);
        result.setRecords(confirmed);
        return result;
    }

    /**
     * 用户名模糊解析为用户 id 集；空名返回 null 表示不过滤，无匹配返回空列表。
     * Resolves a fuzzy user name to user ids; a blank name means no filter (null), no match means empty.
     */
    private List<Long> userIdsByName(String userName) {
        if (StringUtils.isBlank(userName)) {
            return null;
        }
        return userService.lambdaQuery()
                .apply("name ilike {0}", "%" + userName + "%")
                .list()
                .stream()
                .map(User::getId)
                .toList();
    }

    /**
     * 快筛条件：无引用计数+可用行+超保护期，可选文件名/用户/主键上界过滤；sorted 为真时按主键倒序。
     * Quick-screen predicate: zero ref count, not soft-deleted, past the protection period, with
     * optional name/user/id-ceiling filters; ordered by primary key descending when sorted is true.
     */
    private LambdaQueryWrapper<AdiFile> candidatesWrapper(LocalDateTime ttl, String name, List<Long> userIds, Long beforeId, boolean sorted) {
        LambdaQueryWrapper<AdiFile> wrapper = Wrappers.<AdiFile>lambdaQuery()
                .eq(AdiFile::getRefCount, 0)
                .eq(AdiFile::getIsDeleted, false)
                .lt(AdiFile::getCreateTime, ttl);
        if (StringUtils.isNotBlank(name)) {
            wrapper.apply("name ilike {0}", "%" + name + "%");
        }
        if (null != userIds) {
            wrapper.in(AdiFile::getUserId, userIds);
        }
        if (null != beforeId) {
            wrapper.lt(AdiFile::getId, beforeId);
        }
        if (sorted) {
            wrapper.orderByDesc(AdiFile::getId);
        }
        return wrapper;
    }

    /**
     * 管理端手动删除：执行序钉死=①全扫描面实查复核→②>0 拒绝（顺手回写）→③=0 才软删行，path
     * 无其他可用行共享时一并物理删字节。「立即」指不走保留期，不指跳过复核——关掉列表渲染到点击
     * 之间的 TOCTOU 窗口。删除后的软删行由 GC 阶段二统一清运（文件已不在，复核恒 0）。
     * <p>
     * Admin manual delete, fixed order: ① recount across the full scan surface → ② reject on a positive
     * count (writing it back) → ③ only on zero soft-delete the row, deleting the bytes when no
     * other live row shares the path. "Immediate" skips the retention period, never the re-verification —
     * it closes the TOCTOU window between rendering and clicking. The soft-deleted row is later hard-deleted by
     * GC phase two (its file is gone, the recount stays zero).
     * </p>
     */
    public void deleteForCleanup(String uuid) {
        Map<String, Long> refs = countRefs(List.of(uuid));
        long total = refs.getOrDefault(uuid, 0L);
        if (total > 0) {
            fileService.update(Wrappers.<AdiFile>lambdaUpdate()
                    .eq(AdiFile::getUuid, uuid)
                    .set(AdiFile::getRefCount, (int) total));
            throw new BaseException(ErrorEnum.A_FILE_STILL_REFERENCED);
        }
        AdiFile adiFile = fileService.getOne(Wrappers.<AdiFile>lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getIsDeleted, false)
                .last("limit 1"));
        if (null == adiFile) {
            throw new BaseException(ErrorEnum.A_FILE_NOT_EXIST);
        }
        if (!pathSharedByOtherRow(adiFile) && !deletePhysicalFile(adiFile)) {
            throw new BaseException(ErrorEnum.A_FILE_DELETE_FAIL);
        }
        fileService.update(Wrappers.<AdiFile>lambdaUpdate()
                .eq(AdiFile::getId, adiFile.getId())
                .set(AdiFile::getIsDeleted, true));
    }

    /**
     * 重算一组 uuid 的引用，立即回收其中已无引用的文件行。
     * 1. uuid 去重、过滤空值
     * 2. 重算各文件的活跃引用
     * 3. 查出可用行，逐条按重算值处置
     * 处置规则：有引用回写计数并保留；无引用软删行，path 独占时一并物理删字节
     * （物理删失败字节留待 GC 阶段二清运）。重算、行查询、逐行处置均尽力而为，失败只记日志。
     * <p>
     * Recount the references of the given uuids and immediately reclaim the unreferenced rows.
     * 1. deduplicate and drop blank uuids
     * 2. recount the live references per file
     * 3. load the live rows and dispose of each by its recount
     * Disposition: a positive recount writes the count back and keeps the row; zero
     * soft-deletes the row and deletes the bytes when the path is unshared (a
     * failed file deletion leaves the bytes to GC phase two). Best-effort throughout — the
     * recount, the row load and each disposition only log on failure.
     * </p>
     */
    public void reclaimUnreferenced(Collection<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return;
        }
        List<String> distinct = new LinkedHashSet<>(uuids).stream()
                .filter(StringUtils::isNotBlank)
                .toList();
        if (distinct.isEmpty()) {
            return;
        }
        log.info("Reclaim unreferenced files, uuids:{}", distinct);
        Map<String, Long> refs;
        List<AdiFile> rows;
        try {
            refs = countRefs(distinct);
            rows = fileService.list(Wrappers.<AdiFile>lambdaQuery()
                    .in(AdiFile::getUuid, distinct)
                    .eq(AdiFile::getIsDeleted, false));
        } catch (Exception e) {
            log.error("Reclaim unreferenced files fail, uuids:{}", distinct, e);
            return;
        }
        for (AdiFile row : rows) {
            long total = refs.getOrDefault(row.getUuid(), 0L);
            try {
                if (total > 0) {
                    if (null == row.getRefCount() || row.getRefCount() != (int) total) {
                        fileGcMapper.updateRefCount(row.getId(), (int) total);
                    }
                } else {
                    fileGcMapper.softDeleteById(row.getId());
                    if (!pathSharedByOtherRow(row)) {
                        deletePhysicalFile(row);
                    }
                }
            } catch (Exception e) {
                log.error("Reclaim unreferenced file fail, uuid:{}", row.getUuid(), e);
            }
        }
    }

    /**
     * 当前用户删除自己的一个文件：仍有引用返回 false 不动；无引用则软删行，path 无其他可用行
     * 共享时一并物理删字节。
     * <p>
     * Delete one of the current user's files: a still-referenced file returns false untouched;
     * an unreferenced one is soft-deleted, with the bytes deleted when no other
     * live row shares the path.
     * </p>
     */
    public boolean deleteByOwner(String uuid) {
        if (StringUtils.isBlank(uuid)) {
            return false;
        }
        if (countRefs(List.of(uuid)).getOrDefault(uuid, 0L) > 0) {
            return false;
        }
        AdiFile adiFile = fileService.getOne(Wrappers.<AdiFile>lambdaQuery()
                .eq(AdiFile::getUuid, uuid)
                .eq(AdiFile::getUserId, ThreadContext.getCurrentUserId())
                .eq(AdiFile::getIsDeleted, false)
                .last("limit 1"));
        if (null == adiFile) {
            return false;
        }
        if (!pathSharedByOtherRow(adiFile) && !deletePhysicalFile(adiFile)) {
            return false;
        }
        return fileService.update(Wrappers.<AdiFile>lambdaUpdate()
                .eq(AdiFile::getId, adiFile.getId())
                .set(AdiFile::getIsDeleted, true));
    }

    /**
     * path 是否被其他行共享
     * <p>
     * Whether the path is shared by another row.
     * </p>
     */
    private boolean pathSharedByOtherRow(AdiFile adiFile) {
        if (StringUtils.isBlank(adiFile.getPath())) {
            return false;
        }
        return fileService.count(Wrappers.<AdiFile>lambdaQuery()
                .eq(AdiFile::getPath, adiFile.getPath())
                .eq(AdiFile::getIsDeleted, false)
                .ne(AdiFile::getId, adiFile.getId())) > 0;
    }

    /**
     * 全扫描面引用实查：给定 uuid 集合（去重保序）返回各文件的活跃引用数，未出现在结果中的 uuid 引用为 0。
     * <p>
     * Live reference recount across the full scan surface: returns per-uuid counts for the given
     * (deduplicated, order-preserving) collection; uuids absent from the result have zero references.
     * </p>
     */
    public Map<String, Long> countRefs(Collection<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return Map.of();
        }
        List<String> distinct = new LinkedHashSet<>(uuids).stream().toList();
        List<FileRefCountDto> counts = fileGcMapper.countRefs(distinct);
        Map<String, Long> result = new LinkedHashMap<>();
        counts.forEach(item -> result.put(item.getFileUuid(), item.getTotal()));
        return result;
    }

    /**
     * 把一组可用文件行的 ref_count 改写为按业务表活跃引用重算出的真实计数（偏高调低、偏低调高，
     * 双向纠偏）；按批分块、逐块尽力而为——在调用方删除提交之后调用，失败只记日志不向调用方抛出。
     * <p>
     * Rewrite the stored ref_count of the given live file rows to the true count
     * recomputed from live references across the business tables (lowering over-counts and
     * raising under-counts); chunked per batch and best-effort per chunk — called after the
     * caller's delete has committed, failures only log, never propagate.
     * </p>
     */
    public void clampRefCounts(Collection<String> uuids) {
        if (CollectionUtils.isEmpty(uuids)) {
            return;
        }
        List<String> distinct = new LinkedHashSet<>(uuids).stream().toList();
        for (int from = 0; from < distinct.size(); from += BATCH_SIZE) {
            List<String> chunk = distinct.subList(from, Math.min(from + BATCH_SIZE, distinct.size()));
            try {
                clampRefCountChunk(chunk);
            } catch (Exception e) {
                log.error("Clamp ref counts fail, chunk size:{}", chunk.size(), e);
            }
        }
    }

    private void clampRefCountChunk(List<String> chunk) {
        List<AdiFile> rows = fileService.list(Wrappers.<AdiFile>lambdaQuery()
                .in(AdiFile::getUuid, chunk)
                .eq(AdiFile::getIsDeleted, false));
        if (rows.isEmpty()) {
            return;
        }
        Map<String, Long> refs = countRefs(chunk);
        for (AdiFile row : rows) {
            long total = refs.getOrDefault(row.getUuid(), 0L);
            if (null == row.getRefCount() || row.getRefCount() != (int) total) {
                fileGcMapper.updateRefCount(row.getId(), (int) total);
            }
        }
    }

    /**
     * 物理删除文件，失败仅告警返回 false（行保留待重试——先删行会让盘上文件脱离追踪）。
     * <p>
     * Delete the physical file; a failure only warns and returns false so the row survives for
     * the next round — hard-deleting the row first would leave the on-disk file untracked.
     * </p>
     */
    private boolean deletePhysicalFile(AdiFile adiFile) {
        try {
            FileOperatorContext.delete(adiFile);
            return true;
        } catch (Exception e) {
            log.error("Delete physical file fail, uuid:{}, path:{}", adiFile.getUuid(), adiFile.getPath(), e);
            return false;
        }
    }

    /**
     * 探测物理文件是否仍在，异常按不存在处理（行保持软删等待，下轮再试）。
     * <p>
     * Whether the physical file still exists; an error counts as absent (the row keeps waiting
     * for the next round).
     * </p>
     */
    private boolean physicalFileExists(AdiFile adiFile) {
        try {
            return FileOperatorContext.checkIfExist(adiFile);
        } catch (Exception e) {
            log.error("Check physical file fail, uuid:{}", adiFile.getUuid(), e);
            return false;
        }
    }

    private Map<Long, String> userNames(List<AdiFile> records) {
        List<Long> userIds = records.stream().map(AdiFile::getUserId).distinct().toList();
        if (userIds.isEmpty()) {
            return Map.of();
        }
        return userService.listByIds(userIds).stream()
                .collect(Collectors.toMap(User::getId, User::getName, (a, b) -> a));
    }

    private FileCleanupDto toDto(AdiFile adiFile, Map<Long, String> userNames) {
        FileCleanupDto dto = new FileCleanupDto();
        dto.setUuid(adiFile.getUuid());
        dto.setName(adiFile.getName());
        dto.setExt(adiFile.getExt());
        dto.setSize(localSize(adiFile));
        dto.setUrl(FileOperatorContext.getFileUrl(adiFile));
        dto.setUserId(adiFile.getUserId());
        dto.setUserName(userNames.getOrDefault(adiFile.getUserId(), ""));
        dto.setCreateTime(adiFile.getCreateTime());
        return dto;
    }

    /**
     * 本地存储读文件大小；OSS 对象大小需逐对象元数据调用，返回 null 由前端显示占位。
     * <p>
     * File size for local storage; OSS object size would need a per-object metadata call, null lets the
     * frontend render a placeholder.
     * </p>
     */
    private Long localSize(AdiFile adiFile) {
        if (!Objects.equals(adiFile.getStorageLocation(), STORAGE_LOCATION_VALUE_LOCAL) || StringUtils.isBlank(adiFile.getPath())) {
            return null;
        }
        try {
            return Files.size(Paths.get(adiFile.getPath()));
        } catch (Exception e) {
            return null;
        }
    }
}
