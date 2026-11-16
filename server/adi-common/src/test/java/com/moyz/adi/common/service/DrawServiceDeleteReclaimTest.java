package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.DrawMapper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Generated-image references are released by recount-based reclamation after the draw row is
 * gone (or the uuid has left generated_images) — never by owner-scoped file deletion, which
 * silently skipped files belonging to another user and left their ref_count stuck above zero.
 * This test pins the call and its ordering against the draw row update.
 * <p>
 * 生成图引用在 draw 行消亡（或 uuid 移出 generated_images）之后由引用重算回收释放，
 * 不再走按所有者过滤的文件删除——那会静默跳过他人所有的文件，使其 ref_count 停留在虚高值。
 * 本测试钉住调用本身及其与 draw 行更新的先后顺序。
 * </p>
 */
class DrawServiceDeleteReclaimTest {

    private static final long OWNER_ID = 42L;

    private DrawService drawService;
    private DrawMapper drawMapper;
    private FileService fileService;
    private FileGcService fileGcService;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Draw.class);
        drawService = new DrawService();
        drawMapper = mock(DrawMapper.class);
        ReflectionTestUtils.setField(drawService, "baseMapper", drawMapper);
        ReflectionTestUtils.setField(drawService, "entityClass", Draw.class);
        ReflectionTestUtils.setField(drawService, "self", drawService);
        fileService = mock(FileService.class);
        fileGcService = mock(FileGcService.class);
        ReflectionTestUtils.setField(drawService, "fileService", fileService);
        ReflectionTestUtils.setField(drawService, "fileGcService", fileGcService);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
    }

    @Test
    void delReclaimsGeneratedImagesAfterTheDrawRowIsGone() {
        Draw draw = draw(11L, OWNER_ID, "g1,g2");
        when(drawMapper.selectOne(any())).thenReturn(draw);
        ThreadContext.setCurrentUser(user(OWNER_ID, false));

        assertTrue(drawService.del("some-draw"));

        InOrder order = inOrder(drawMapper, fileGcService);
        order.verify(drawMapper).update(any(), any());
        order.verify(fileGcService).reclaimUnreferenced(List.of("g1", "g2"));
        assertTrue(sqlSet(drawMapper).contains("is_deleted"));
    }

    @Test
    void delStillDetachesInputSideReferences() {
        Draw draw = draw(11L, OWNER_ID, "");
        draw.setOriginalImage("input-uuid");
        when(drawMapper.selectOne(any())).thenReturn(draw);
        ThreadContext.setCurrentUser(user(OWNER_ID, false));

        assertTrue(drawService.del("some-draw"));

        verify(fileService).detachRefs(List.of("input-uuid"));
    }

    @Test
    void delGeneratedFileReclaimsOnlyTheRemovedImageAfterTheRowUpdate() {
        Draw draw = draw(11L, OWNER_ID, "g1,g2");
        when(drawMapper.selectOne(any())).thenReturn(draw);
        ThreadContext.setCurrentUser(user(OWNER_ID, false));

        assertTrue(drawService.delGeneratedFile("some-draw", "g1"));

        InOrder order = inOrder(drawMapper, fileGcService);
        order.verify(drawMapper).update(any(), any());
        order.verify(fileGcService).reclaimUnreferenced(List.of("g1"));
        assertTrue(setValues(drawMapper).contains("g2"));
    }

    @Test
    void delGeneratedFileDoesNotReclaimWhenTheUuidIsNotPresent() {
        Draw draw = draw(11L, OWNER_ID, "g1,g2");
        when(drawMapper.selectOne(any())).thenReturn(draw);
        ThreadContext.setCurrentUser(user(OWNER_ID, false));

        assertTrue(drawService.delGeneratedFile("some-draw", "g3"));

        verify(fileGcService, never()).reclaimUnreferenced(any());
    }

    private String sqlSet(DrawMapper mapper) {
        ArgumentCaptor<LambdaUpdateWrapper<Draw>> captor = ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(mapper).update(any(), captor.capture());
        return captor.getValue().getSqlSet();
    }

    private Collection<Object> setValues(DrawMapper mapper) {
        ArgumentCaptor<LambdaUpdateWrapper<Draw>> captor = ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(mapper).update(any(), captor.capture());
        LambdaUpdateWrapper<Draw> wrapper = captor.getValue();
        wrapper.getSqlSet();
        return wrapper.getParamNameValuePairs().values();
    }

    private Draw draw(Long id, Long userId, String generatedImages) {
        Draw draw = new Draw();
        draw.setId(id);
        draw.setUuid("some-draw");
        draw.setUserId(userId);
        draw.setGeneratedImages(generatedImages);
        draw.setIsDeleted(false);
        return draw;
    }

    private User user(Long id, boolean admin) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(admin);
        return user;
    }
}
