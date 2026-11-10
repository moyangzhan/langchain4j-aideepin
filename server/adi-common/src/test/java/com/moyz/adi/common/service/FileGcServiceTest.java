package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.dto.FileRefCountDto;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.mapper.FileGcMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * GC orchestration contract: the switch is read live from the database (missing key = off),
 * phase one soft-deletes only zero-recount rows and writes back positive recounts, phase two
 * re-verifies every expired row (purge / revive / keep), and the admin delete path never skips
 * the recount. Cursor batching keeps both loops finite even when rows stay in place.
 * <p>
 * GC 编排契约：开关每轮直查库（缺键=关）；阶段一只软删复核为零的行、复核为正一律回写；
 * 阶段二对每条到期行先复核再三分（清运/复活/保持）；管理端删除路径绝不跳过复核。
 * 游标分批保证两个循环在行原地不动时仍有限终止。
 * </p>
 */
class FileGcServiceTest {

    private FileGcService fileGcService;
    private FileGcMapper fileGcMapper;
    private FileService fileService;
    private SysConfigService sysConfigService;
    private UserService userService;
    private MockedStatic<FileOperatorContext> fileOperatorContext;

    @BeforeAll
    static void initTableInfo() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), AdiFile.class);
    }

    @BeforeEach
    void setUp() {
        fileGcService = new FileGcService();
        fileGcMapper = mock(FileGcMapper.class);
        fileService = mock(FileService.class);
        sysConfigService = mock(SysConfigService.class);
        userService = mock(UserService.class);
        ReflectionTestUtils.setField(fileGcService, "fileGcMapper", fileGcMapper);
        ReflectionTestUtils.setField(fileGcService, "fileService", fileService);
        ReflectionTestUtils.setField(fileGcService, "sysConfigService", sysConfigService);
        ReflectionTestUtils.setField(fileGcService, "userService", userService);
        fileOperatorContext = mockStatic(FileOperatorContext.class);
        when(sysConfigService.getRawByKey(AdiConstant.SysConfigKey.FILE_GC_ENABLED)).thenReturn("true");
        when(fileService.update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(true);
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        MessageSource messageSource = mock(MessageSource.class);
        when(applicationContext.getBean(MessageSource.class)).thenReturn(messageSource);
        when(messageSource.getMessage(any(String.class), any(), any()))
                .thenAnswer(invocation -> invocation.getArgument(0));
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", applicationContext);
    }

    @AfterEach
    void tearDown() {
        fileOperatorContext.close();
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
    }

    @Test
    void missingSwitchKeyMeansOff() {
        when(sysConfigService.getRawByKey(AdiConstant.SysConfigKey.FILE_GC_ENABLED)).thenReturn(null);
        assertTrue(!fileGcService.gcEnabled());
    }

    @Test
    void gcStopsEntirelyWhenSwitchIsOff() {
        when(sysConfigService.getRawByKey(AdiConstant.SysConfigKey.FILE_GC_ENABLED)).thenReturn("false");

        fileGcService.gc();

        verifyNoInteractions(fileGcMapper);
    }

    @Test
    void phaseOneSoftDeletesZeroRowsAndHealsPositiveRows() {
        AdiFile zero = file(1L, "zero-uuid");
        AdiFile positive = file(2L, "positive-uuid");
        when(fileGcMapper.listUnreferencedCandidates(anyLong(), any(LocalDateTime.class), anyInt()))
                .thenReturn(List.of(zero, positive))
                .thenReturn(List.of());
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of(refCount("positive-uuid", 3L)));

        fileGcService.gc();

        verify(fileGcMapper).softDeleteById(1L);
        verify(fileGcMapper).updateRefCount(2L, 3);
        verify(fileGcMapper, times(2)).listUnreferencedCandidates(anyLong(), any(LocalDateTime.class), anyInt());
    }

    @Test
    void phaseTwoPurgesRevivesAndKeepsAccordingToRecountAndPhysicalFile() {
        AdiFile purgeMe = file(1L, "purge-uuid");
        AdiFile reviveMe = file(2L, "revive-uuid");
        AdiFile keepMe = file(3L, "keep-uuid");
        when(fileGcMapper.listUnreferencedCandidates(anyLong(), any(LocalDateTime.class), anyInt())).thenReturn(List.of());
        when(fileGcMapper.listGraceExpired(anyLong(), any(LocalDateTime.class), anyInt()))
                .thenReturn(List.of(purgeMe, reviveMe, keepMe))
                .thenReturn(List.of());
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of(
                refCount("revive-uuid", 2L),
                refCount("keep-uuid", 5L)));
        fileOperatorContext.when(() -> FileOperatorContext.delete(purgeMe)).then(invocation -> null);
        fileOperatorContext.when(() -> FileOperatorContext.checkIfExist(reviveMe)).thenReturn(true);
        fileOperatorContext.when(() -> FileOperatorContext.checkIfExist(keepMe)).thenReturn(false);

        fileGcService.gc();

        verify(fileGcMapper).deleteById(1L);
        verify(fileGcMapper).reviveById(2L, 2);
        verify(fileGcMapper, never()).deleteById(2L);
        verify(fileGcMapper, never()).reviveById(eq(3L), anyInt());
        verify(fileGcMapper, never()).deleteById(3L);
        fileOperatorContext.verify(() -> FileOperatorContext.delete(purgeMe));
    }

    @Test
    void physicalDeleteFailureKeepsTheRowForTheNextRound() {
        AdiFile doomed = file(1L, "doomed-uuid");
        when(fileGcMapper.listUnreferencedCandidates(anyLong(), any(LocalDateTime.class), anyInt())).thenReturn(List.of());
        when(fileGcMapper.listGraceExpired(anyLong(), any(LocalDateTime.class), anyInt()))
                .thenReturn(List.of(doomed))
                .thenReturn(List.of());
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of());
        fileOperatorContext.when(() -> FileOperatorContext.delete(doomed)).thenThrow(new RuntimeException("oss down"));

        fileGcService.gc();

        verify(fileGcMapper, never()).deleteById(1L);
    }

    @Test
    void adminDeleteFailsVisiblyWhenPhysicalDeleteFails() {
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of());
        AdiFile target = file(9L, "stuck-uuid");
        when(fileService.getOne(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(target);
        fileOperatorContext.when(() -> FileOperatorContext.delete(target)).thenThrow(new RuntimeException("disk error"));

        BaseException e = assertThrows(BaseException.class, () -> fileGcService.deleteForCleanup("stuck-uuid"));

        assertEquals(ErrorEnum.A_FILE_DELETE_FAIL.getCode(), e.getCode());
        verify(fileService, never()).update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class));
    }

    @Test
    void adminDeleteRejectsReferencedFileAndWritesBackTheRecount() {
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of(refCount("busy-uuid", 4L)));

        BaseException e = assertThrows(BaseException.class, () -> fileGcService.deleteForCleanup("busy-uuid"));

        assertEquals(ErrorEnum.A_FILE_STILL_REFERENCED.getCode(), e.getCode());
        verify(fileService).update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class));
    }

    @Test
    void adminDeletePhysicallyDeletesAndSoftDeletesOnlyAfterZeroRecount() {
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of());
        AdiFile target = file(9L, "free-uuid");
        when(fileService.getOne(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(target);

        fileGcService.deleteForCleanup("free-uuid");

        fileOperatorContext.verify(() -> FileOperatorContext.delete(target));
        verify(fileService, times(1)).update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class));
    }

    @Test
    void adminSearchExcludesAndHealsPositiveRowsOnThePage() {
        AdiFile zero = file(1L, "zero-uuid");
        AdiFile positive = file(2L, "positive-uuid");
        Page<AdiFile> dbPage = batch(zero, positive);
        Page<AdiFile> emptyBatch = batch();
        when(fileService.page(any(Page.class), any(com.baomidou.mybatisplus.core.conditions.Wrapper.class)))
                .thenReturn(dbPage)
                .thenReturn(emptyBatch);
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of(refCount("positive-uuid", 2L)));
        when(userService.listByIds(anyCollection())).thenReturn(List.of(user(1L, "alice")));
        fileOperatorContext.when(() -> FileOperatorContext.getFileUrl(zero)).thenReturn("/file/zero-uuid.png");

        Page<com.moyz.adi.common.dto.FileCleanupDto> page = fileGcService.search(1, 10, null, null);

        assertEquals(1, page.getRecords().size());
        assertEquals("zero-uuid", page.getRecords().get(0).getUuid());
        assertEquals("/file/zero-uuid.png", page.getRecords().get(0).getUrl());
        assertEquals("alice", page.getRecords().get(0).getUserName());
        verify(fileGcMapper).updateRefCount(2L, 2);
    }

    @Test
    void adminSearchFillsThePageAcrossBatches() {
        AdiFile referenced1 = file(1L, "ref1-uuid");
        AdiFile first = file(2L, "first-uuid");
        AdiFile second = file(3L, "second-uuid");
        AdiFile referenced2 = file(4L, "ref2-uuid");
        AdiFile third = file(5L, "third-uuid");
        when(fileService.page(any(Page.class), any(com.baomidou.mybatisplus.core.conditions.Wrapper.class)))
                .thenReturn(batch(referenced1, first, second))
                .thenReturn(batch(referenced2, third))
                .thenReturn(batch());
        when(fileGcMapper.countRefs(anyList()))
                .thenReturn(List.of(refCount("ref1-uuid", 1L)))
                .thenReturn(List.of(refCount("ref2-uuid", 2L)));
        when(fileService.count(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(9L);
        when(userService.listByIds(anyCollection())).thenReturn(List.of(user(1L, "alice")));
        fileOperatorContext.when(() -> FileOperatorContext.getFileUrl(first)).thenReturn("/file/first.png");
        fileOperatorContext.when(() -> FileOperatorContext.getFileUrl(second)).thenReturn("/file/second.png");
        fileOperatorContext.when(() -> FileOperatorContext.getFileUrl(third)).thenReturn("/file/third.png");

        Page<com.moyz.adi.common.dto.FileCleanupDto> page = fileGcService.search(1, 3, null, null);

        assertEquals(List.of("first-uuid", "second-uuid", "third-uuid"),
                page.getRecords().stream().map(com.moyz.adi.common.dto.FileCleanupDto::getUuid).toList());
        assertEquals(9L, page.getTotal());
        verify(fileGcMapper).updateRefCount(1L, 1);
        verify(fileGcMapper).updateRefCount(4L, 2);
    }

    @Test
    void adminSearchCountWrapperStaysUnorderedSoCountStarIsValid() {
        when(fileService.page(any(Page.class), any(com.baomidou.mybatisplus.core.conditions.Wrapper.class)))
                .thenReturn(batch());
        when(fileService.count(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(0L);

        fileGcService.search(1, 10, null, null);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<com.baomidou.mybatisplus.core.conditions.Wrapper<AdiFile>> countCaptor =
                ArgumentCaptor.forClass(com.baomidou.mybatisplus.core.conditions.Wrapper.class);
        verify(fileService).count(countCaptor.capture());
        assertFalse(countCaptor.getValue().getSqlSegment().contains("ORDER BY"));
    }

    @Test
    void adminSearchPagingWrapperKeepsPrimaryKeyDescendingOrder() {
        when(fileService.page(any(Page.class), any(com.baomidou.mybatisplus.core.conditions.Wrapper.class)))
                .thenReturn(batch());
        when(fileService.count(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(0L);

        fileGcService.search(1, 10, null, null);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<com.baomidou.mybatisplus.core.conditions.Wrapper<AdiFile>> pageCaptor =
                ArgumentCaptor.forClass(com.baomidou.mybatisplus.core.conditions.Wrapper.class);
        verify(fileService).page(any(Page.class), pageCaptor.capture());
        assertTrue(pageCaptor.getValue().getSqlSegment().contains("ORDER BY"));
    }

    @Test
    void countRefsDeduplicatesUuidsBeforeQuerying() {
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of());

        fileGcService.countRefs(List.of("a", "b", "a"));

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<String>> captor = ArgumentCaptor.forClass(List.class);
        verify(fileGcMapper).countRefs(captor.capture());
        assertEquals(List.of("a", "b"), captor.getValue());
    }

    @Test
    void clampRefCountsWritesTheLiveRecountForStaleRowsOnly() {
        AdiFile staleHigh = file(1L, "stale-high-uuid");
        staleHigh.setRefCount(3);
        AdiFile balanced = file(2L, "balanced-uuid");
        balanced.setRefCount(2);
        AdiFile stalePositiveButUnreferenced = file(3L, "stuck-uuid");
        stalePositiveButUnreferenced.setRefCount(1);
        when(fileService.list(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class)))
                .thenReturn(List.of(staleHigh, balanced, stalePositiveButUnreferenced));
        when(fileGcMapper.countRefs(anyList())).thenReturn(List.of(refCount("stale-high-uuid", 1L), refCount("balanced-uuid", 2L)));

        fileGcService.clampRefCounts(List.of("stale-high-uuid", "balanced-uuid", "stuck-uuid"));

        verify(fileGcMapper).updateRefCount(1L, 1);
        verify(fileGcMapper).updateRefCount(3L, 0);
        verify(fileGcMapper, never()).updateRefCount(eq(2L), anyInt());
    }

    private static Page<AdiFile> batch(AdiFile... files) {
        Page<AdiFile> page = new Page<>(1, 200, files.length);
        page.setRecords(List.of(files));
        return page;
    }

    private static AdiFile file(Long id, String uuid) {
        AdiFile adiFile = new AdiFile();
        adiFile.setId(id);
        adiFile.setUuid(uuid);
        adiFile.setUserId(1L);
        adiFile.setStorageLocation(1);
        adiFile.setIsDeleted(false);
        adiFile.setCreateTime(LocalDateTime.now().minusDays(30));
        return adiFile;
    }

    private static FileRefCountDto refCount(String uuid, Long total) {
        FileRefCountDto dto = new FileRefCountDto();
        dto.setFileUuid(uuid);
        dto.setTotal(total);
        return dto;
    }

    private static User user(Long id, String name) {
        User user = new User();
        user.setId(id);
        user.setName(name);
        return user;
    }
}
