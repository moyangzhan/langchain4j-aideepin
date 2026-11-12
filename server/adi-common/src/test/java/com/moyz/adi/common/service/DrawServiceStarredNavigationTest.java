package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.DrawDto;
import com.moyz.adi.common.entity.Draw;
import com.moyz.adi.common.entity.DrawStar;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.DrawMapper;
import com.moyz.adi.common.mapper.DrawStarMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * "Previous / next favorite" walks the caller's own favorites by the moment the caller starred
 * them. The gallery detail page calls these while a starred image is open, so a wrong scope (the
 * draw owner's stars) or a wrong direction lands the user on an unrelated image — or, when the
 * caller has no further favorite, on a NullPointerException instead of "end of list".
 * <p>
 * 收藏夹里的「上一张 / 下一张」按「我收藏的时间」在我自己的收藏里走：
 * 范围错了（用图片属主的收藏）或方向错了（向前却查更晚的）都会跳到不相干的图，
 * 而没有相邻项时应当返回 null 表示到底，不是抛空指针。
 */
class DrawServiceStarredNavigationTest {

    private static final long VIEWER_ID = 7L;
    private static final long OWNER_ID = 42L;
    private static final long OLDER_DRAW_ID = 10L;
    private static final long CURRENT_DRAW_ID = 11L;
    private static final long NEWER_DRAW_ID = 12L;

    private static final LocalDateTime OLDER_STARRED_AT = LocalDateTime.of(2026, 1, 1, 9, 0);
    private static final LocalDateTime CURRENT_STARRED_AT = LocalDateTime.of(2026, 1, 2, 9, 0);
    private static final LocalDateTime NEWER_STARRED_AT = LocalDateTime.of(2026, 1, 3, 9, 0);

    private DrawService drawService;
    private DrawMapper drawMapper;
    private DrawStarMapper drawStarMapper;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Draw.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), DrawStar.class);

        drawService = new DrawService();
        drawMapper = mock(DrawMapper.class);
        drawStarMapper = mock(DrawStarMapper.class);
        DrawStarService drawStarService = new DrawStarService();
        ReflectionTestUtils.setField(drawStarService, "baseMapper", drawStarMapper);
        ReflectionTestUtils.setField(drawStarService, "entityClass", DrawStar.class);

        ReflectionTestUtils.setField(drawService, "baseMapper", drawMapper);
        ReflectionTestUtils.setField(drawService, "entityClass", Draw.class);
        ReflectionTestUtils.setField(drawService, "drawStarService", drawStarService);
        ReflectionTestUtils.setField(drawService, "fileService", mock(FileService.class));
        ReflectionTestUtils.setField(drawService, "userService", mock(UserService.class));

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
    void newerMovesForwardInsideTheCallersFavorites() {
        stubDatabase(null);
        ThreadContext.setCurrentUser(user(VIEWER_ID));
        ThreadContext.setToken("viewer-token");

        DrawDto next = drawService.newerStarredOne("current-draw");

        assertEquals("newer-draw", next.getUuid());
    }

    @Test
    void olderMovesBackwardInsideTheCallersFavorites() {
        stubDatabase(null);
        ThreadContext.setCurrentUser(user(VIEWER_ID));
        ThreadContext.setToken("viewer-token");

        DrawDto previous = drawService.olderStarredOne("current-draw");

        assertEquals("older-draw", previous.getUuid());
    }

    @Test
    void noFurtherFavoriteEndsTheListInsteadOfFailing() {
        stubDatabase(NEWER_STARRED_AT);
        ThreadContext.setCurrentUser(user(VIEWER_ID));
        ThreadContext.setToken("viewer-token");

        // The caller's newest favorite is the one on screen: there is nothing after it.
        assertNull(drawService.newerStarredOne("current-draw"));
    }

    /**
     * Stand-in for the database. The star table holds the caller's three favorites only, so a
     * query scoped to any other user returns nothing — that is what catches a lookup that uses the
     * draw owner's id. Which neighbour the query may see depends on the direction it asks for:
     * forward only sees the later star, backward only sees the earlier one.
     *
     * @param hideNewerStar when set, the later star is dropped from the fixture — the case where the
     *                      image on screen is already the newest favorite
     */
    private void stubDatabase(LocalDateTime hideNewerStar) {
        when(drawMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            if (query.getSqlSegment().contains("uuid")) {
                return draw(CURRENT_DRAW_ID, "current-draw");
            }
            if (query.getParamNameValuePairs().containsValue(NEWER_DRAW_ID)) {
                return draw(NEWER_DRAW_ID, "newer-draw");
            }
            return draw(OLDER_DRAW_ID, "older-draw");
        });
        when(drawStarMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            if (query.getParamNameValuePairs().containsValue(OWNER_ID)) {
                // Nobody starred anything as the draw's owner: only the viewer has favorites.
                return null;
            }
            String sql = query.getSqlSegment();
            if (sql.contains("update_time >")) {
                return hideNewerStar == null ? star(NEWER_DRAW_ID, NEWER_STARRED_AT) : null;
            }
            if (sql.contains("update_time <")) {
                return star(OLDER_DRAW_ID, OLDER_STARRED_AT);
            }
            return star(CURRENT_DRAW_ID, CURRENT_STARRED_AT);
        });
    }

    private DrawStar star(Long drawId, LocalDateTime starredAt) {
        DrawStar star = new DrawStar();
        star.setId(drawId);
        star.setUserId(VIEWER_ID);
        star.setDrawId(drawId);
        star.setUpdateTime(starredAt);
        star.setIsDeleted(false);
        return star;
    }

    private Draw draw(Long id, String uuid) {
        Draw draw = new Draw();
        draw.setId(id);
        draw.setUuid(uuid);
        draw.setUserId(OWNER_ID);
        draw.setIsPublic(true);
        draw.setIsDeleted(false);
        draw.setStarCount(1);
        draw.setAiModelId(999L);
        draw.setAiModelName("some-model");
        return draw;
    }

    private User user(Long id) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(false);
        user.setName("user-" + id);
        return user;
    }
}
