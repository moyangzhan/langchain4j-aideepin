package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
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

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Regenerating a failed draw re-runs the image model and writes the result back into the draw row,
 * so it may only reach a row the caller owns. Its siblings in the same service — {@code del},
 * {@code delGeneratedFile} and {@code setDrawPublic} — all read the row through
 * {@code PrivilegeUtil}; this test pins that {@code regenerate} does too.
 * <p>
 * 重新生成会把图片模型再跑一遍并把结果写回那行 draw 记录，因此只能作用于调用方自己的记录。
 * 同一个 service 里的 del / delGeneratedFile / setDrawPublic 都通过 PrivilegeUtil 取行，
 * 本测试钉住 regenerate 也必须如此。
 */
class DrawServiceRegenerateAuthorizationTest {

    private static final long OWNER_ID = 42L;

    private DrawService drawService;
    private DrawMapper drawMapper;
    private DrawService self;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Draw.class);
        drawService = new DrawService();
        drawMapper = mock(DrawMapper.class);
        self = mock(DrawService.class);
        ReflectionTestUtils.setField(drawService, "baseMapper", drawMapper);
        ReflectionTestUtils.setField(drawService, "entityClass", Draw.class);
        ReflectionTestUtils.setField(drawService, "self", self);

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
    void regenerateRejectsADrawTheCallerDoesNotOwn() {
        // Stand-in for the database: the row only comes back when the query has already been
        // scoped to the caller. Without the owner condition a row belonging to someone else
        // is what a real database would hand over.
        when(drawMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            String sql = query.getSqlSegment();
            assertTrue(sql.contains("uuid"), "uuid condition missing: " + sql);
            assertTrue(sql.contains("process_status"), "failed-only condition missing: " + sql);
            return sql.contains("user_id") ? null : failedDrawOfAnotherUser();
        });
        ThreadContext.setCurrentUser(user(7L, false));

        BaseException exception = assertThrows(BaseException.class,
                () -> drawService.regenerate("foreign-draw"));

        assertEquals("B0005", exception.getCode());
        verify(self, never()).createFromRemote(any(), any());
    }

    @Test
    void regenerateStillReachesTheOwnersFailedDraw() {
        when(drawMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            return query.getSqlSegment().contains("user_id") ? failedDrawOfAnotherUser() : null;
        });
        ThreadContext.setCurrentUser(user(OWNER_ID, false));

        drawService.regenerate("foreign-draw");

        verify(self).createFromRemote(any(Draw.class), any(User.class));
    }

    @Test
    void regenerateStillReachesAnyoneDrawForAnAdmin() {
        when(drawMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            return query.getSqlSegment().contains("user_id") ? null : failedDrawOfAnotherUser();
        });
        ThreadContext.setCurrentUser(user(7L, true));

        drawService.regenerate("foreign-draw");

        verify(self).createFromRemote(any(Draw.class), any(User.class));
    }

    private Draw failedDrawOfAnotherUser() {
        Draw draw = new Draw();
        draw.setId(11L);
        draw.setUuid("foreign-draw");
        draw.setUserId(OWNER_ID);
        draw.setProcessStatus(AdiConstant.GenerateImage.STATUS_FAIL);
        draw.setIsDeleted(false);
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
