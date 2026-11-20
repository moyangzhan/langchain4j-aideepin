package com.moyz.adi.common.service;

import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.dto.RefEmbeddingDto;
import com.moyz.adi.common.entity.CharacterMemoryEpisodic;
import com.moyz.adi.common.entity.CharacterMemoryInstruction;
import com.moyz.adi.common.entity.CharacterMemorySemantic;
import com.moyz.adi.common.entity.CharacterMessageRefMemoryEmbedding;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.MemoryType;
import com.moyz.adi.common.mapper.CharacterMessageRefMemoryEmbeddingMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 记忆弹窗读切换契约钉子：ref 按桶路由回各自关系表（uuid 短形查询）、内容与结构化字段
 * （事件类型/创建时间/重要性）来自关系行、记忆行缺失的 ref 跳过、instruction 路由码不炸弹窗；
 * 命中计数 XML 保持「软删行不计 + uuid 批量 +1」的语句形态。
 * <p>
 * Pins for the memory popup read switch: refs route by bucket to their relational table
 * (queried by short uuid), content and structured fields (event type / create time /
 * importance) come from the relational row, refs whose memory row is gone are skipped, a
 * stray instruction routing code cannot crash the popup; the hit-count XML keeps the
 * "skip soft-deleted, batch +1 by uuid" statement shape.
 */
class CharacterMemoryPopupTest {

    private CharacterMessageRefMemoryEmbeddingMapper mapper;
    private CharacterMemorySemanticService semanticService;
    private CharacterMemoryEpisodicService episodicService;
    private CharacterMemoryInstructionService instructionService;
    private CharacterMessageRefMemoryEmbeddingService service;

    @BeforeEach
    void setUp() {
        ThreadContext.setCurrentUser(user(42L));
        mapper = mock(CharacterMessageRefMemoryEmbeddingMapper.class);
        semanticService = mock(CharacterMemorySemanticService.class);
        episodicService = mock(CharacterMemoryEpisodicService.class);
        instructionService = mock(CharacterMemoryInstructionService.class);
        service = new CharacterMessageRefMemoryEmbeddingService();
        ReflectionTestUtils.setField(service, "baseMapper", mapper);
        ReflectionTestUtils.setField(service, "characterMemorySemanticService", semanticService);
        ReflectionTestUtils.setField(service, "characterMemoryEpisodicService", episodicService);
        ReflectionTestUtils.setField(service, "characterMemoryInstructionService", instructionService);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unload();
    }

    @Test
    void bucketsResolveAgainstTheirRelationalTablesWithShortUuids() {
        String semanticUuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b0";
        String episodicUuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b1";
        when(mapper.listByMsgUuid("m", 42L)).thenReturn(List.of(
                ref(canonical(semanticUuid), MemoryType.SEMANTIC),
                ref(canonical(episodicUuid), MemoryType.EPISODIC)));
        when(semanticService.mapByUuids(anyList())).thenReturn(Map.of(semanticUuid, semanticRow(semanticUuid, "用户住在杭州")));
        when(episodicService.mapByUuids(anyList())).thenReturn(Map.of(episodicUuid, episodicRow(episodicUuid, "用户去了北京")));

        List<RefEmbeddingDto> dtos = service.listRefEmbeddings("m");

        assertEquals(2, dtos.size());
        RefEmbeddingDto semanticDto = dtos.stream().filter(d -> "semantic".equals(d.getMemoryType())).findFirst().orElseThrow();
        assertEquals("用户住在杭州", semanticDto.getText());
        assertEquals(canonical(semanticUuid), semanticDto.getEmbeddingId());
        RefEmbeddingDto episodicDto = dtos.stream().filter(d -> "episodic".equals(d.getMemoryType())).findFirst().orElseThrow();
        assertEquals("用户去了北京", episodicDto.getText());
        assertEquals("travel", episodicDto.getEventType());
        assertEquals("2026-11-20 19:00:00", episodicDto.getCreateTime());
        assertEquals(4, episodicDto.getImportance());
    }

    @Test
    void refsWhoseMemoryRowIsGoneAreSkipped() {
        String deadUuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b2";
        String aliveUuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b3";
        when(mapper.listByMsgUuid("m", 42L)).thenReturn(List.of(
                ref(canonical(deadUuid), MemoryType.SEMANTIC),
                ref(canonical(aliveUuid), MemoryType.SEMANTIC)));
        when(semanticService.mapByUuids(anyList())).thenReturn(Map.of(aliveUuid, semanticRow(aliveUuid, "活着的事实")));

        List<RefEmbeddingDto> dtos = service.listRefEmbeddings("m");

        assertEquals(1, dtos.size());
        assertEquals("活着的事实", dtos.get(0).getText());
    }

    @Test
    void instructionRefsResolveAgainstTheirRelationalTable() {
        String instructionUuid = "0193a0b0c0d0e0f0a0b0c0d0e0f0a0b4";
        when(mapper.listByMsgUuid("m", 42L)).thenReturn(List.of(
                ref(canonical(instructionUuid), MemoryType.INSTRUCTION)));
        CharacterMemoryInstruction row = new CharacterMemoryInstruction();
        row.setUuid(instructionUuid);
        row.setContent("回答代码问题时先给结论");
        when(instructionService.mapByUuids(anyList())).thenReturn(Map.of(instructionUuid, row));

        List<RefEmbeddingDto> dtos = service.listRefEmbeddings("m");

        assertEquals(1, dtos.size());
        assertEquals("回答代码问题时先给结论", dtos.get(0).getText());
        assertEquals("instruction", dtos.get(0).getMemoryType());
    }

    @Test
    void instructionRefsWithoutRowAreSkipped() {
        when(mapper.listByMsgUuid("m", 42L)).thenReturn(List.of(
                ref(canonical("0193a0b0c0d0e0f0a0b0c0d0e0f0a0b5"), MemoryType.INSTRUCTION)));
        when(instructionService.mapByUuids(anyList())).thenReturn(Map.of());

        assertEquals(List.of(), service.listRefEmbeddings("m"));
    }

    @Test
    void hitCountXmlSkipsSoftDeletedAndIncrementsByUuid() throws Exception {
        for (String xmlPath : new String[]{"mapper/CharacterMemorySemanticMapper.xml", "mapper/CharacterMemoryEpisodicMapper.xml"}) {
            String increment = statement(xmlPath, "incrementHitCountByUuids");
            assertTrue(increment.contains("hit_count = hit_count + 1"), xmlPath + " increment missing");
            assertTrue(increment.contains("is_deleted = false"), xmlPath + " soft-delete filter missing");
            assertTrue(increment.contains("uuid in"), xmlPath + " uuid predicate missing");
        }
    }

    private static CharacterMessageRefMemoryEmbedding ref(String embeddingId, MemoryType type) {
        CharacterMessageRefMemoryEmbedding ref = new CharacterMessageRefMemoryEmbedding();
        ref.setEmbeddingId(embeddingId);
        ref.setMemoryType(type);
        return ref;
    }

    private static CharacterMemorySemantic semanticRow(String uuid, String content) {
        CharacterMemorySemantic row = new CharacterMemorySemantic();
        row.setUuid(uuid);
        row.setContent(content);
        return row;
    }

    private static CharacterMemoryEpisodic episodicRow(String uuid, String content) {
        CharacterMemoryEpisodic row = new CharacterMemoryEpisodic();
        row.setUuid(uuid);
        row.setContent(content);
        row.setEventType("travel");
        row.setImportance(4);
        row.setCreateTime(LocalDateTime.of(2026, 11, 20, 19, 0, 0));
        return row;
    }

    private static String statement(String xmlPath, String id) throws Exception {
        try (InputStream in = CharacterMemoryPopupTest.class.getClassLoader().getResourceAsStream(xmlPath)) {
            assertNotNull(in, xmlPath + " not on classpath");
            var doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            var nodes = doc.getDocumentElement().getElementsByTagName("*");
            for (int i = 0; i < nodes.getLength(); i++) {
                Element e = (Element) nodes.item(i);
                if (id.equals(e.getAttribute("id"))) {
                    return e.getTextContent().replaceAll("\\s+", " ");
                }
            }
        }
        throw new IllegalStateException("statement not found: " + id);
    }

    private static String canonical(String shortUuid) {
        return new StringBuilder(36)
                .append(shortUuid, 0, 8).append('-')
                .append(shortUuid, 8, 12).append('-')
                .append(shortUuid, 12, 16).append('-')
                .append(shortUuid, 16, 20).append('-')
                .append(shortUuid, 20, 32)
                .toString();
    }

    private static User user(Long id) {
        User user = new User();
        user.setId(id);
        return user;
    }
}
