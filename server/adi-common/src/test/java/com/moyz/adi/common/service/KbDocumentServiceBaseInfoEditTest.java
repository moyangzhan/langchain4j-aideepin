package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.KbDocumentBaseEditReq;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.mapper.KbDocumentMapper;
import com.moyz.adi.common.util.SpringUtil;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.MessageSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 管理端文档基础信息编辑：仅更新标题/摘要、不触发索引失效与重索引；标题类元数据不参与失效模型。
 * Pins the admin-side document base-info edit: only title/brief are written, with no index
 * invalidation or re-enqueue side effects (title-like metadata is excluded from invalidation).
 */
class KbDocumentServiceBaseInfoEditTest {

    private KbDocumentService service;
    private KbDocumentMapper mapper;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "fixture"), KbDocument.class);
        service = new KbDocumentService();
        mapper = mock(KbDocumentMapper.class);
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        MessageSource messageSource = mock(MessageSource.class);
        when(applicationContext.getBean(MessageSource.class)).thenReturn(messageSource);
        when(messageSource.getMessage(any(String.class), any(), any())).thenAnswer(invocation -> invocation.getArgument(0));
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", applicationContext);
        ReflectionTestUtils.setField(service, "baseMapper", mapper);
        ReflectionTestUtils.setField(service, "entityClass", KbDocument.class);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
        ReflectionTestUtils.setField(SpringUtil.class, "applicationContext", null);
    }

    @Test
    void adminEditsTitleAndBriefOnly() {
        KbDocument doc = doc(7L, "doc-uuid", "old title", "old brief", "remark text");
        when(mapper.selectOne(any())).thenReturn(doc);
        when(mapper.updateById(any(KbDocument.class))).thenReturn(1);
        ThreadContext.setCurrentUser(user(1L, true));

        assertTrue(service.updateBaseInfo(req("doc-uuid", "new title", "new brief")));

        ArgumentCaptor<KbDocument> captor = ArgumentCaptor.forClass(KbDocument.class);
        verify(mapper).updateById(captor.capture());
        assertEquals(7L, captor.getValue().getId());
        assertEquals("new title", captor.getValue().getTitle());
        assertEquals("new brief", captor.getValue().getBrief());
        assertNull(captor.getValue().getRemark());
    }

    @Test
    void blankBriefIsDerivedFromRemark() {
        KbDocument doc = doc(7L, "doc-uuid", "old title", "", "remark text");
        when(mapper.selectOne(any())).thenReturn(doc);
        when(mapper.updateById(any(KbDocument.class))).thenReturn(1);
        ThreadContext.setCurrentUser(user(1L, true));

        assertTrue(service.updateBaseInfo(req("doc-uuid", "new title", " ")));

        ArgumentCaptor<KbDocument> captor = ArgumentCaptor.forClass(KbDocument.class);
        verify(mapper).updateById(captor.capture());
        assertEquals("remark text", captor.getValue().getBrief());
    }

    @Test
    void deletedDocumentIsNotFound() {
        when(mapper.selectOne(any())).thenReturn(null);
        ThreadContext.setCurrentUser(user(1L, true));

        assertThrows(BaseException.class, () -> service.updateBaseInfo(req("doc-uuid", "new title", "brief")));
        verify(mapper, never()).updateById(any(KbDocument.class));
    }

    @Test
    void nonOwnerWithoutAdminIsRejected() {
        when(mapper.checkWritePrivilege(anyString(), anyLong())).thenReturn(0);
        ThreadContext.setCurrentUser(user(2L, false));

        assertThrows(BaseException.class, () -> service.updateBaseInfo(req("doc-uuid", "new title", "brief")));
        verify(mapper, never()).updateById(any(KbDocument.class));
    }

    private static KbDocument doc(Long id, String uuid, String title, String brief, String remark) {
        KbDocument doc = new KbDocument();
        doc.setId(id);
        doc.setUuid(uuid);
        doc.setTitle(title);
        doc.setBrief(brief);
        doc.setRemark(remark);
        return doc;
    }

    private static KbDocumentBaseEditReq req(String uuid, String title, String brief) {
        KbDocumentBaseEditReq req = new KbDocumentBaseEditReq();
        req.setUuid(uuid);
        req.setTitle(title);
        req.setBrief(brief);
        return req;
    }

    private static User user(Long id, boolean isAdmin) {
        User user = new User();
        user.setId(id);
        user.setIsAdmin(isAdmin);
        return user;
    }
}
