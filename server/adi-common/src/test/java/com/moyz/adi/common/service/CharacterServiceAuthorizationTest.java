package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.CharacterMsgListResp;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.CharacterMessage;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.ErrorEnum;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.CharacterMapper;
import com.moyz.adi.common.mapper.CharacterMessageMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class CharacterServiceAuthorizationTest {

    private CharacterService characterService;
    private CharacterMapper characterMapper;
    private CharacterMessageMapper messageMapper;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Character.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), CharacterMessage.class);
        characterService = new CharacterService();
        characterMapper = mock(CharacterMapper.class);
        messageMapper = mock(CharacterMessageMapper.class);
        CharacterMessageService messageService = new CharacterMessageService();
        ReflectionTestUtils.setField(characterService, "baseMapper", characterMapper);
        ReflectionTestUtils.setField(characterService, "entityClass", Character.class);
        ReflectionTestUtils.setField(messageService, "baseMapper", messageMapper);
        ReflectionTestUtils.setField(messageService, "entityClass", CharacterMessage.class);
        ReflectionTestUtils.setField(characterService, "characterMessageService", messageService);
        ReflectionTestUtils.setField(characterService, "llmCallRecordService", mock(LLMCallRecordService.class));

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

    @ParameterizedTest
    @ValueSource(strings = {"", "known-message"})
    void detailRejectsNonOwnerBeforeReadingMessages(String cursor) {
        when(characterMapper.selectOne(any())).thenReturn(character());
        ThreadContext.setCurrentUser(user(7L, false));

        BaseException exception = assertThrows(BaseException.class,
                () -> characterService.detail("private-character", cursor, 10));

        assertEquals(ErrorEnum.A_USER_NOT_AUTH.getCode(), exception.getCode());
        verifyNoInteractions(messageMapper);
    }

    @Test
    void detailReturnsMessagesToOwner() {
        assertMessagesReadableBy(user(42L, false));
    }

    @Test
    void detailReturnsMessagesToAdmin() {
        assertMessagesReadableBy(user(7L, true));
    }

    @Test
    void detailRejectsMissingCharacterBeforeReadingMessages() {
        ThreadContext.setCurrentUser(user(42L, false));

        BaseException exception = assertThrows(BaseException.class,
                () -> characterService.detail("missing-character", "", 10));

        assertEquals(ErrorEnum.A_CHARACTER_NOT_EXIST.getCode(), exception.getCode());
        verifyNoInteractions(messageMapper);
    }

    @Test
    void detailScopesCursorLookupToAuthorizedCharacter() {
        when(characterMapper.selectOne(any())).thenReturn(character());
        when(messageMapper.selectOne(any())).thenAnswer(invocation -> {
            AbstractWrapper<?, ?, ?> query = invocation.getArgument(0);
            assertTrue(query.getSqlSegment().contains("character_id"));
            assertTrue(query.getParamNameValuePairs().containsValue(11L));
            CharacterMessage cursor = new CharacterMessage();
            cursor.setId(101L);
            return cursor;
        });
        ThreadContext.setCurrentUser(user(42L, false));

        CharacterMsgListResp response = characterService.detail("private-character", "current-cursor", 10);

        assertEquals("", response.getMinMsgUuid());
        assertEquals(Collections.emptyList(), response.getMsgList());
    }

    private void assertMessagesReadableBy(User currentUser) {
        when(characterMapper.selectOne(any())).thenReturn(character());
        CharacterMessage question = new CharacterMessage();
        question.setId(101L);
        question.setUuid("private-question");
        question.setCharacterId(11L);
        question.setUserId(42L);
        question.setParentMessageId(0L);
        question.setRemark("Private question");
        question.setIsDeleted(false);
        when(messageMapper.selectList(any())).thenReturn(List.of(question), Collections.emptyList());
        ThreadContext.setCurrentUser(currentUser);

        CharacterMsgListResp response = characterService.detail("private-character", "", 10);

        assertEquals("private-question", response.getMinMsgUuid());
        assertEquals(1, response.getMsgList().size());
        assertEquals("Private question", response.getMsgList().get(0).getRemark());
        assertEquals(Collections.emptyList(), response.getMsgList().get(0).getChildren());
    }

    private static Character character() {
        Character character = new Character();
        character.setId(11L);
        character.setUuid("private-character");
        character.setUserId(42L);
        character.setIsDeleted(false);
        return character;
    }

    private static User user(Long id, boolean isAdmin) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(isAdmin);
        return user;
    }
}
