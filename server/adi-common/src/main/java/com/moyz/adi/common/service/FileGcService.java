package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
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
 * 无引用文件回收：两步判定（ref_count=0 快筛 → 业务表实查复核）+ 两段式删除（软删宽限 → 满月物理删+硬删行）。
 * 实查>0 一律回写 ref_count（自愈假零漂移，最危险方向）；每轮运行开始直查数据库读开关，missing key 即停。
 * 同时承载管理端手动清理的判定核心：列表实时计算，删除前再次复核。
 * <p>
 * Unreferenced-file GC: two-step detection (ref_count=0 prescreen → live recount across business tables) plus
 * two-stage deletion (soft-delete grace → physical delete + row purge after one month). Any positive recount
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
     * 软删宽限期（月）：机器判定的可逆后悔窗口，期满物理删+硬删行。
     * Grace period in months for soft-deleted rows; expired rows are physically deleted and purged.
     */
    private static final int GRACE_MONTHS = 1;

    /**
     * 分批大小：候选复核、到期清运与手动列表实查的批粒度。
     * Batch size shared by candidate verification, grace purging and the manual list recount.
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
        purgeExpired();
    }

    /**
     * 阶段一：候选判定与软删。实查=0 软删进宽限期（update_time 由触发器刷新为宽限起点）；
     * 实查>0 不删且回写计数。游标分批，回写/软删均滑出候选条件，循环收敛。
     * <p>
     * Phase one: candidate detection and soft delete. Zero recount soft-deletes into the grace period
     * (the trigger refreshes update_time as the grace clock); a positive recount stays and writes back.
     * Cursor-batched; both outcomes slide the row out of the predicate so the loop converges.
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
     * 阶段二：宽限期满清运。每行删除前再次复核：实查=0 → 物理删文件+硬删行（物理删失败保留行，下轮重试
     * ——先删行会让盘上文件脱离追踪）；实查>0 且物理文件仍在 → 复活并回写（宽限期内出现新引用）；
     * 实查>0 但文件已不在 → 引用指向已物理删除的文件，行保持软删待引用消散后清运。
     * 保持软删的行不滑出条件，靠游标保证有限。
     * <p>
     * Phase two: purge expired grace rows. Each row is re-verified before deletion: zero recount →
     * physically delete the file and purge the row (a failed physical delete keeps the row for the next
     * round — purging the row first would leave the on-disk file untracked); positive recount with the
     * physical file present → revive and write back (a new reference arrived inside the grace window); positive
     * recount with the file gone → the reference points at a dead file, the row stays soft-deleted until the
     * reference dissolves. Rows that stay soft-deleted never leave the predicate — the cursor keeps the loop finite.
     * </p>
     */
    void purgeExpired() {
        LocalDateTime before = LocalDateTime.now().minusMonths(GRACE_MONTHS);
        long lastId = 0L;
        int purged = 0;
        int revived = 0;
        while (true) {
            List<AdiFile> expired = fileGcMapper.listGraceExpired(lastId, before, BATCH_SIZE);
            if (expired.isEmpty()) {
                break;
            }
            lastId = expired.get(expired.size() - 1).getId();
            Map<String, Long> refs = countRefs(expired.stream().map(AdiFile::getUuid).toList());
            for (AdiFile adiFile : expired) {
                long total = refs.getOrDefault(adiFile.getUuid(), 0L);
                if (total == 0) {
                    if (physicalDelete(adiFile)) {
                        fileGcMapper.deleteById(adiFile.getId());
                        purged++;
                    }
                } else if (physicalFileExists(adiFile)) {
                    fileGcMapper.reviveById(adiFile.getId(), (int) total);
                    revived++;
                    log.info("File revived inside grace window, uuid:{}, refs:{}", adiFile.getUuid(), total);
                } else {
                    log.warn("Soft-deleted file still referenced but physical file is gone, keep waiting, uuid:{}, refs:{}",
                            adiFile.getUuid(), total);
                }
            }
        }
        if (purged > 0 || revived > 0) {
            log.info("File GC phase two finished, purged:{}, revived:{}", purged, revived);
        }
    }

    /**
     * 管理端手动清理列表：按快筛条件分批取候选，逐批实查复核——有引用的剔除并回写计数（自愈假零漂移），
     * 剔除后继续向后取候选直到凑满当前页或候选耗尽；total 为快筛计数，回写治愈的行随后离开候选集。
     * 支持文件名与用户名模糊过滤。
     * <p>
     * Admin manual-cleanup list: quick-screened candidate batches, each recounted live — referenced rows
     * are excluded with their counts written back (self-healing the false-zero drift) — and fetching
     * continues until the page is full or candidates run out. The total stays the quick-screen count;
     * healed rows leave the candidate set afterwards. Fuzzy name/user-name filters supported.
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
     * 快筛条件：无引用计数+未软删+超保护期，可选文件名/用户/主键上界过滤；sorted 为真时按主键倒序。
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
     * 管理端手动删除：执行序钉死=①全扫描面实查复核→②>0 拒绝（顺手回写）→③=0 才物理删+软删行。
     * 「立即」指不走宽限期，不指跳过复核——关掉列表渲染到点击之间的 TOCTOU 窗口。
     * 删除后的软删行由 GC 阶段二统一清运（文件已不在，复核恒 0）。
     * <p>
     * Admin manual delete, fixed order: ① recount across the full scan surface → ② reject on a positive
     * count (writing it back) → ③ only on zero physically delete and soft-delete the row. "Immediate" skips
     * the grace period, never the re-verification — it closes the TOCTOU window between rendering and clicking.
     * The soft-deleted row is later purged by GC phase two (its file is gone, the recount stays zero).
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
        if (!physicalDelete(adiFile)) {
            throw new BaseException(ErrorEnum.A_FILE_DELETE_FAIL);
        }
        fileService.update(Wrappers.<AdiFile>lambdaUpdate()
                .eq(AdiFile::getId, adiFile.getId())
                .set(AdiFile::getIsDeleted, true));
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
     * 把一组活行文件的 ref_count 钳制为当前实查值（双向）：URL 形态的工作流产物引用没有 detach 路径，
     * 引用消散后回写自愈只覆盖假零方向，正向卡值在运行删除点由此收敛。按批分块、逐块尽力而为——
     * 钳制发生在业务删除提交之后，失败只记日志不再向调用方抛出。
     * <p>
     * Clamp the ref_count of the given live rows to the current recount (both directions):
     * URL-shaped workflow-product references have no detach path, so once they dissolve the
     * write-back self-heal — which only covers the false-zero direction — cannot bring the stuck
     * positive counts down; this closes that gap at runtime-deletion points. Chunked per batch and
     * best-effort per chunk — the clamp runs after the caller's delete has committed, so failures
     * only log, never propagate.
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
     * Physically delete the file; a failure only warns and returns false so the row survives for
     * the next round — purging the row first would leave the on-disk file untracked.
     * </p>
     */
    private boolean physicalDelete(AdiFile adiFile) {
        try {
            FileOperatorContext.delete(adiFile);
            return true;
        } catch (Exception e) {
            log.error("Physical delete file fail, uuid:{}, path:{}", adiFile.getUuid(), adiFile.getPath(), e);
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
