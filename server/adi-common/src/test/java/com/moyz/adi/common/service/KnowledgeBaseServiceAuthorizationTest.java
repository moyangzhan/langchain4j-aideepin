package com.moyz.adi.common.service;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.KnowledgeBaseQa;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class KnowledgeBaseServiceAuthorizationTest {

    private KnowledgeBaseService knowledgeBaseService;
    private KnowledgeBaseMapper knowledgeBaseMapper;
    private KnowledgeBaseQaService knowledgeBaseQaService;

    @BeforeEach
    void setUp() {
        knowledgeBaseService = new KnowledgeBaseService();
        knowledgeBaseMapper = mock(KnowledgeBaseMapper.class);
        knowledgeBaseQaService = mock(KnowledgeBaseQaService.class);
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        MessageSource messageSource = mock(MessageSource.class);
        when(applicationContext.getBean(MessageSource.class)).thenReturn(messageSource);
        when(messageSource.getMessage(any(String.class), any(), any())).thenAnswer(invocation -> invocation.getArgument(0));
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", applicationContext);
        ReflectionTestUtils.setField(knowledgeBaseService, "baseMapper", knowledgeBaseMapper);
        ReflectionTestUtils.setField(knowledgeBaseService, "knowledgeBaseQaRecordService", knowledgeBaseQaService);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
    }

    @Test
    void privateKnowledgeBaseRejectsNonOwner() {
        KnowledgeBase knowledgeBase = knowledgeBase(11L, 42L, false);
        when(knowledgeBaseMapper.selectOne(any())).thenReturn(knowledgeBase);
        ThreadContext.setCurrentUser(user(7L, false));

        BaseException exception = assertThrows(BaseException.class,
                () -> knowledgeBaseService.getReadableOrThrow("private-kb"));

        assertEquals(ErrorEnum.A_DATA_NOT_FOUND.getCode(), exception.getCode());
    }

    @Test
    void privateKnowledgeBaseAllowsOwnerAndAdmin() {
        KnowledgeBase knowledgeBase = knowledgeBase(11L, 42L, false);
        when(knowledgeBaseMapper.selectOne(any())).thenReturn(knowledgeBase);

        ThreadContext.setCurrentUser(user(42L, false));
        assertSame(knowledgeBase, knowledgeBaseService.getReadableOrThrow("private-kb"));

        ThreadContext.setCurrentUser(user(7L, true));
        assertSame(knowledgeBase, knowledgeBaseService.getReadableOrThrow("private-kb"));
    }

    @Test
    void publicKnowledgeBaseAllowsAnyAuthenticatedUser() {
        KnowledgeBase knowledgeBase = knowledgeBase(11L, 42L, true);
        when(knowledgeBaseMapper.selectOne(any())).thenReturn(knowledgeBase);
        ThreadContext.setCurrentUser(user(7L, false));

        assertDoesNotThrow(() -> knowledgeBaseService.getReadableOrThrow("public-kb"));
    }

    @Test
    void qaReadChecksTheOwningKnowledgeBase() {
        KnowledgeBaseQa qaRecord = new KnowledgeBaseQa();
        qaRecord.setKbUuid("private-kb");
        when(knowledgeBaseQaService.getOrThrow("qa-record")).thenReturn(qaRecord);
        when(knowledgeBaseMapper.selectOne(any())).thenReturn(knowledgeBase(11L, 42L, false));
        ThreadContext.setCurrentUser(user(7L, false));

        BaseException exception = assertThrows(BaseException.class,
                () -> knowledgeBaseService.getReadableQaOrThrow("qa-record"));

        assertEquals(ErrorEnum.A_DATA_NOT_FOUND.getCode(), exception.getCode());
    }

    private static KnowledgeBase knowledgeBase(Long id, Long ownerId, boolean isPublic) {
        KnowledgeBase knowledgeBase = new KnowledgeBase();
        knowledgeBase.setId(id);
        knowledgeBase.setUuid("kb");
        knowledgeBase.setOwnerId(ownerId);
        knowledgeBase.setIsPublic(isPublic);
        knowledgeBase.setIsDeleted(false);
        return knowledgeBase;
    }

    private static User user(Long id, boolean isAdmin) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(isAdmin);
        return user;
    }
}
