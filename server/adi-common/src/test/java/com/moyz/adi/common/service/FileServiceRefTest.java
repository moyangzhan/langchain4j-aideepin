package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.entity.AdiFile;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.mapper.FileMapper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Reference registration contract: attach increments atomically in SQL (never a read-modify-write),
 * revives a soft-deleted row only when its physical file still exists, treats missing rows/files as
 * dead uuids (warn only, business never blocked); detach decrements with a zero floor and skips
 * soft-deleted rows.
 * <p>
 * 引用登记契约：attach 为 SQL 原子自增（绝不做读改写）；软删行仅在物理文件仍在时复活；
 * 行缺失或文件已删视为死引用（仅告警不阻断业务）；detach 地板为零且跳过软删行。
 * </p>
 */
class FileServiceRefTest {

    private FileService fileService;
    private FileMapper fileMapper;
    private org.mockito.MockedStatic<FileOperatorContext> fileOperatorContext;

    @BeforeAll
    static void initTableInfo() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), AdiFile.class);
    }

    @BeforeEach
    void setUp() {
        fileService = new FileService();
        fileMapper = mock(FileMapper.class);
        ReflectionTestUtils.setField(fileService, "baseMapper", fileMapper);
        ReflectionTestUtils.setField(fileService, "entityClass", AdiFile.class);
        fileOperatorContext = mockStatic(FileOperatorContext.class);
    }

    @AfterEach
    void tearDown() {
        fileOperatorContext.close();
    }

    @Test
    void attachIncrementsAtomicallyOnALiveRow() {
        when(fileMapper.selectOne(any(Wrapper.class))).thenReturn(file("live-uuid", false));

        fileService.attachRef("live-uuid");

        String sqlSet = capturedSqlSet();
        assertTrue(sqlSet.contains("ref_count = ref_count + 1"), sqlSet);
        assertFalse(sqlSet.contains("is_deleted"), "live-row attach must not touch is_deleted: " + sqlSet);
    }

    @Test
    void attachRevivesASoftDeletedRowOnlyWhenThePhysicalFileSurvives() {
        AdiFile graceRow = file("grace-uuid", true);
        when(fileMapper.selectOne(any(Wrapper.class))).thenReturn(graceRow);
        fileOperatorContext.when(() -> FileOperatorContext.checkIfExist(graceRow)).thenReturn(true);

        fileService.attachRef("grace-uuid");

        String sqlSet = capturedSqlSet();
        assertTrue(sqlSet.contains("ref_count = ref_count + 1"), sqlSet);
        assertTrue(sqlSet.contains("is_deleted"), "revive must clear is_deleted: " + sqlSet);
    }

    @Test
    void attachTreatsSoftDeletedRowWithGoneFileAsDeadReference() {
        AdiFile tombstone = file("tombstone-uuid", true);
        when(fileMapper.selectOne(any(Wrapper.class))).thenReturn(tombstone);
        fileOperatorContext.when(() -> FileOperatorContext.checkIfExist(tombstone)).thenReturn(false);

        fileService.attachRef("tombstone-uuid");

        verify(fileMapper, never()).update(any(), any(Wrapper.class));
    }

    @Test
    void attachTreatsMissingRowAsDeadReference() {
        when(fileMapper.selectOne(any(Wrapper.class))).thenReturn(null);

        fileService.attachRef("no-such-uuid");

        verify(fileMapper, never()).update(any(), any(Wrapper.class));
    }

    @Test
    void attachSkipsBlankUuid() {
        fileService.attachRef("");

        verify(fileMapper, never()).selectOne(any(Wrapper.class));
    }

    @Test
    void detachDecrementsWithZeroFloorAndSkipsSoftDeletedRows() {
        when(fileMapper.update(any(), any(Wrapper.class))).thenReturn(1);

        fileService.detachRef("any-uuid");

        String sqlSet = capturedSqlSet();
        String where = capturedWhereSegment();
        assertTrue(sqlSet.contains("GREATEST(ref_count - 1, 0)"), sqlSet);
        assertTrue(where.contains("is_deleted"), "detach must scope to live rows: " + where);
    }

    @SuppressWarnings("unchecked")
    private String capturedSqlSet() {
        return capturedUpdateWrapper().getSqlSet();
    }

    private String capturedWhereSegment() {
        return capturedUpdateWrapper().getSqlSegment();
    }

    @SuppressWarnings("unchecked")
    private com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper<AdiFile> capturedUpdateWrapper() {
        ArgumentCaptor<Wrapper<AdiFile>> captor = ArgumentCaptor.forClass(Wrapper.class);
        verify(fileMapper).update(any(), captor.capture());
        return (com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper<AdiFile>) captor.getValue();
    }

    private static AdiFile file(String uuid, boolean deleted) {
        AdiFile adiFile = new AdiFile();
        adiFile.setId(7L);
        adiFile.setUuid(uuid);
        adiFile.setIsDeleted(deleted);
        adiFile.setRefCount(0);
        return adiFile;
    }
}
