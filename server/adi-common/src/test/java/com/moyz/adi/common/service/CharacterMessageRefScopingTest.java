package com.moyz.adi.common.service;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.RefGraphDto;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.mapper.CharacterMessageRefEmbeddingMapper;
import com.moyz.adi.common.mapper.CharacterMessageRefGraphMapper;
import com.moyz.adi.common.mapper.CharacterMessageRefMemoryEmbeddingMapper;
import com.moyz.adi.common.service.embedding.ICharacterMemoryEmbeddingService;
import com.moyz.adi.common.service.embedding.IEpisodicMemoryEmbeddingService;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * The ref mappers scope their message join to the calling user, so a foreign message
 * uuid yields no rows no matter which caller reaches the service — the guard travels
 * with the SQL, not with controllers. These tests pin both halves of that contract:
 * the XML must keep the scoping conditions, and the services must thread the current
 * user id into the mapper call.
 * <p>
 * 引用 mapper 的消息联表按当前用户过滤，外部用户的消息 uuid 无论从哪个调用方进来都查不到行——
 * 守卫在 SQL 里而不是在 controller 里。本测试钉住契约的两半：XML 必须保留过滤条件，
 * service 必须把当前用户 id 传进 mapper 调用。
 */
class CharacterMessageRefScopingTest {

    @BeforeEach
    void setUp() {
        ThreadContext.setCurrentUser(user(42L));
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "mapper/CharacterMessageRefEmbeddingMapper.xml",
            "mapper/CharacterMessageRefGraphMapper.xml",
            "mapper/CharacterMessageRefMemoryEmbeddingMapper.xml"
    })
    void refQueriesScopeMessageJoinToCurrentUser(String xmlPath) throws Exception {
        String template = soleStatement(xmlPath);
        String prepared = template
                .replaceAll("#\\{[^}]+}", "?")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replaceAll("\\s+", " ")
                .trim();

        assertTrue(prepared.contains("b.uuid = ?"), "message uuid condition missing: " + prepared);
        assertTrue(prepared.contains("b.user_id = ?"), "owner scoping missing: " + prepared);
        assertTrue(prepared.contains("b.is_deleted = false"), "live-message filter missing: " + prepared);
        CCJSqlParserUtil.parse(prepared);
    }

    @Test
    void embeddingReadThreadsCurrentUserIntoMapper() {
        CharacterMessageRefEmbeddingMapper mapper = mock(CharacterMessageRefEmbeddingMapper.class);
        when(mapper.listByMsgUuid("some-message", 42L)).thenReturn(List.of());
        CharacterMessageRefEmbeddingService service = new CharacterMessageRefEmbeddingService();
        ReflectionTestUtils.setField(service, "baseMapper", mapper);

        assertEquals(List.of(), service.listRefEmbeddings("some-message"));

        verify(mapper).listByMsgUuid("some-message", 42L);
    }

    @Test
    void memoryReadThreadsCurrentUserIntoMapperAndSkipsVectorStoresWhenEmpty() {
        CharacterMessageRefMemoryEmbeddingMapper mapper = mock(CharacterMessageRefMemoryEmbeddingMapper.class);
        when(mapper.listByMsgUuid("some-message", 42L)).thenReturn(List.of());
        ICharacterMemoryEmbeddingService semanticStore = mock(ICharacterMemoryEmbeddingService.class);
        IEpisodicMemoryEmbeddingService episodicStore = mock(IEpisodicMemoryEmbeddingService.class);
        CharacterMessageRefMemoryEmbeddingService service = new CharacterMessageRefMemoryEmbeddingService();
        ReflectionTestUtils.setField(service, "baseMapper", mapper);
        ReflectionTestUtils.setField(service, "characterMemoryEmbeddingService", semanticStore);
        ReflectionTestUtils.setField(service, "episodicMemoryEmbeddingService", episodicStore);

        assertEquals(List.of(), service.listRefEmbeddings("some-message"));

        verify(mapper).listByMsgUuid("some-message", 42L);
        verifyNoInteractions(semanticStore, episodicStore);
    }

    @Test
    void graphReadThreadsCurrentUserIntoMapper() {
        CharacterMessageRefGraphMapper mapper = mock(CharacterMessageRefGraphMapper.class);
        when(mapper.listByMsgUuid("some-message", 42L)).thenReturn(List.of());
        CharacterMessageRefGraphService service = new CharacterMessageRefGraphService();
        ReflectionTestUtils.setField(service, "baseMapper", mapper);

        RefGraphDto result = service.getByMsgUuid("some-message");

        assertNotNull(result);
        assertEquals(List.of(), result.getVertices());
        assertEquals(List.of(), result.getEdges());
        verify(mapper).listByMsgUuid("some-message", 42L);
    }

    private static String soleStatement(String xmlPath) throws Exception {
        try (InputStream in = CharacterMessageRefScopingTest.class.getClassLoader().getResourceAsStream(xmlPath)) {
            assertNotNull(in, xmlPath + " not on classpath");
            var doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            var nodes = doc.getDocumentElement().getElementsByTagName("select");
            assertEquals(1, nodes.getLength(), xmlPath + " expected exactly one select");
            return ((Element) nodes.item(0)).getTextContent();
        }
    }

    private static User user(Long id) {
        User user = new User();
        user.setId(id);
        return user;
    }
}
