package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.entity.Character;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.entity.Workflow;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.CharacterMapper;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.mapper.WorkflowMapper;
import com.moyz.adi.common.util.AesUtil;
import com.moyz.adi.common.util.SpringUtil;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * An external API key keeps answering on behalf of the account that owns the resource. Deleting
 * that account has to revoke every key shape it ever issued — including the resource-level keys
 * (character / knowledge base / workflow), which used to only check that the row exists.
 * <p>
 * 外部 API Key 是代某个账号行使权限的：账号被删之后，它发出去的所有 Key 都必须失效，
 * 包括绑定在角色 / 知识库 / 工作流上的资源级 Key（这类以前只判断“行还在不在”）。
 */
class ExtApiServiceValidateApiKeyTest {

    private static final String RAW_KEY = "ext-test-key";
    private static final String AES_KEY = "0123456789abcdef";
    private static final long OWNER_ID = 42L;
    private static final String RESOURCE_UUID = "resource-uuid";

    private ExtApiService extApiService;
    private UserService userService;
    private CharacterMapper characterMapper;
    private KnowledgeBaseMapper knowledgeBaseMapper;
    private WorkflowMapper workflowMapper;
    private String previousAesKey;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Character.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), KnowledgeBase.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), Workflow.class);

        previousAesKey = AesUtil.AES_KEY;
        AesUtil.AES_KEY = AES_KEY;

        extApiService = new ExtApiService();
        userService = mock(UserService.class);
        characterMapper = mock(CharacterMapper.class);
        knowledgeBaseMapper = mock(KnowledgeBaseMapper.class);
        workflowMapper = mock(WorkflowMapper.class);

        CharacterService characterService = new CharacterService();
        ReflectionTestUtils.setField(characterService, "baseMapper", characterMapper);
        ReflectionTestUtils.setField(characterService, "entityClass", Character.class);
        KnowledgeBaseService knowledgeBaseService = new KnowledgeBaseService();
        ReflectionTestUtils.setField(knowledgeBaseService, "baseMapper", knowledgeBaseMapper);
        ReflectionTestUtils.setField(knowledgeBaseService, "entityClass", KnowledgeBase.class);
        WorkflowService workflowService = new WorkflowService();
        ReflectionTestUtils.setField(workflowService, "baseMapper", workflowMapper);
        ReflectionTestUtils.setField(workflowService, "entityClass", Workflow.class);

        ReflectionTestUtils.setField(extApiService, "characterService", characterService);
        ReflectionTestUtils.setField(extApiService, "knowledgeBaseService", knowledgeBaseService);
        ReflectionTestUtils.setField(extApiService, "workflowService", workflowService);
        ReflectionTestUtils.setField(extApiService, "userService", userService);
        ReflectionTestUtils.setField(extApiService, "userExtApiKeyService", mock(UserExtApiKeyService.class));

        ApplicationContext applicationContext = mock(ApplicationContext.class);
        MessageSource messageSource = mock(MessageSource.class);
        when(applicationContext.getBean(MessageSource.class)).thenReturn(messageSource);
        when(messageSource.getMessage(any(String.class), any(), any()))
                .thenAnswer(invocation -> invocation.getArgument(0));
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", applicationContext);
    }

    @AfterEach
    void tearDown() {
        AesUtil.AES_KEY = previousAesKey;
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
    }

    @Test
    void characterKeyOfADeletedAccountIsRejected() {
        when(characterMapper.selectOne(any())).thenReturn(character(OWNER_ID));
        when(userService.getById(OWNER_ID)).thenReturn(user(OWNER_ID, true));

        assertInvalidKey("character");
    }

    @Test
    void knowledgeBaseKeyOfADeletedAccountIsRejected() {
        when(knowledgeBaseMapper.selectOne(any())).thenReturn(knowledgeBase(OWNER_ID));
        when(userService.getById(OWNER_ID)).thenReturn(user(OWNER_ID, true));

        assertInvalidKey("knowledge");
    }

    @Test
    void workflowKeyOfADeletedAccountIsRejected() {
        when(workflowMapper.selectOne(any())).thenReturn(workflow(OWNER_ID));
        when(userService.getById(OWNER_ID)).thenReturn(user(OWNER_ID, true));

        assertInvalidKey("workflow");
    }

    @Test
    void characterKeyOfAnActiveAccountStillResolves() {
        when(characterMapper.selectOne(any())).thenReturn(character(OWNER_ID));
        when(userService.getById(OWNER_ID)).thenReturn(user(OWNER_ID, false));

        ExtApiService.ValidateResult result = extApiService.validateApiKey(RAW_KEY, "character");

        assertEquals(RESOURCE_UUID, result.entityUuid());
        assertNotNull(result.ownerUser());
    }

    private void assertInvalidKey(String type) {
        BaseException exception = assertThrows(BaseException.class, () -> extApiService.validateApiKey(RAW_KEY, type));
        assertEquals("A0063", exception.getCode());
    }

    private Character character(Long ownerId) {
        Character character = new Character();
        character.setUuid(RESOURCE_UUID);
        character.setUserId(ownerId);
        character.setApiKey("encrypted-key");
        return character;
    }

    private KnowledgeBase knowledgeBase(Long ownerId) {
        KnowledgeBase knowledgeBase = new KnowledgeBase();
        knowledgeBase.setUuid(RESOURCE_UUID);
        knowledgeBase.setOwnerId(ownerId);
        knowledgeBase.setApiKey("encrypted-key");
        return knowledgeBase;
    }

    private Workflow workflow(Long ownerId) {
        Workflow workflow = new Workflow();
        workflow.setUuid(RESOURCE_UUID);
        workflow.setUserId(ownerId);
        workflow.setApiKey("encrypted-key");
        return workflow;
    }

    private User user(Long id, boolean deleted) {
        User user = new User();
        user.setId(id);
        user.setName("user-" + id);
        user.setIsDeleted(deleted);
        return user;
    }
}
