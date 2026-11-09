package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.DrawDto;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.DrawMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The star button in the gallery opens a public draw owned by someone else, so favoriting has to
 * follow the read rule (owner or public) rather than the ownership rule, and the {@code isStar}
 * flag it renders has to be the viewer's, not the owner's.
 * <p>
 * 画廊里点开的公开图属于别人，因此收藏要按「读权限」判定（属主或公开），
 * 且返回给前端的 isStar 必须是当前查看者的状态，而不是图片属主的。
 */
class DrawServiceStarTest {

    private static final long VIEWER_ID = 7L;
    private static final long OWNER_ID = 42L;
    private static final long DRAW_ID = 11L;

    private DrawService drawService;
    private DrawMapper drawMapper;
    private DrawStarService drawStarService;
    private FileService fileService;
    private UserService userService;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Draw.class);
        drawService = new DrawService();
        drawMapper = mock(DrawMapper.class);
        drawStarService = mock(DrawStarService.class);
        fileService = mock(FileService.class);
        userService = mock(UserService.class);
        ReflectionTestUtils.setField(drawService, "baseMapper", drawMapper);
        ReflectionTestUtils.setField(drawService, "entityClass", Draw.class);
        ReflectionTestUtils.setField(drawService, "drawStarService", drawStarService);
        ReflectionTestUtils.setField(drawService, "fileService", fileService);
        ReflectionTestUtils.setField(drawService, "userService", userService);

        ApplicationContext applicationContext = mock(ApplicationContext.class);
        MessageSource messageSource = mock(MessageSource.class);
        when(applicationContext.getBean(MessageSource.class)).thenReturn(messageSource);
        when(messageSource.getMessage(any(String.class), any(), any()))
                .thenAnswer(invocation -> invocation.getArgument(0));
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", applicationContext);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
    }

    @Test
    void starringAPublicDrawOfAnotherUserIsAllowed() {
        stubDrawIsReachableOnlyWithoutAnOwnerCondition(draw(OWNER_ID, true));
        when(drawStarService.isStarred(DRAW_ID, VIEWER_ID)).thenReturn(true);
        ThreadContext.setCurrentUser(user(VIEWER_ID, false));
        ThreadContext.setToken("viewer-token");

        drawService.toggleStar("public-draw");

        // The star is recorded for the caller, on the draw that belongs to someone else.
        verify(drawStarService).toggle(DRAW_ID, VIEWER_ID);
    }

    @Test
    void starringAPrivateDrawOfAnotherUserIsStillRejected() {
        stubDrawIsReachableOnlyWithoutAnOwnerCondition(draw(OWNER_ID, false));
        ThreadContext.setCurrentUser(user(VIEWER_ID, false));
        ThreadContext.setToken("viewer-token");

        BaseException exception = assertThrows(BaseException.class, () -> drawService.toggleStar("private-draw"));

        // Reported as "not found", like every other refusal in this service, so the caller cannot
        // probe whether the draw exists.
        assertEquals("A0060", exception.getCode());
        verify(drawStarService, never()).toggle(anyLong(), anyLong());
    }

    @Test
    void isStarOnAPublicDrawReflectsTheViewerNotTheOwner() {
        // The owner starred their own draw; the viewer has not. The detail payload the viewer
        // receives must not inherit the owner's flag, otherwise the star icon shows "starred"
        // and the first click silently un-stars instead of starring.
        stubDrawIsReachableOnlyWithoutAnOwnerCondition(draw(OWNER_ID, true));
        when(drawStarService.isStarred(DRAW_ID, OWNER_ID)).thenReturn(true);
        when(drawStarService.isStarred(DRAW_ID, VIEWER_ID)).thenReturn(false);
        ThreadContext.setCurrentUser(user(VIEWER_ID, false));
        ThreadContext.setToken("viewer-token");

        DrawDto detail = drawService.getPublicOrMine("public-draw");

        assertFalse(detail.getIsStar(), "isStar must reflect the viewer, not the draw owner");
    }

    /**
     * Stand-in for the database: another user's row is only reachable when the query is not
     * scoped to a user, which is exactly the difference between a readable lookup and an
     * ownership lookup.
     */
    private void stubDrawIsReachableOnlyWithoutAnOwnerCondition(Draw row) {
        when(drawMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            String sql = query.getSqlSegment();
            assertTrue(sql.contains("uuid"), "uuid condition missing: " + sql);
            return sql.contains("user_id") ? null : row;
        });
    }

    private Draw draw(Long ownerId, boolean isPublic) {
        Draw draw = new Draw();
        draw.setId(DRAW_ID);
        draw.setUuid(isPublic ? "public-draw" : "private-draw");
        draw.setUserId(ownerId);
        draw.setIsPublic(isPublic);
        draw.setIsDeleted(false);
        draw.setStarCount(1);
        draw.setAiModelId(999L);
        draw.setAiModelName("some-model");
        draw.setPrompt("a cat");
        return draw;
    }

    private User user(Long id, boolean admin) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(admin);
        user.setName("user-" + id);
        return user;
    }
}
