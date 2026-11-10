package com.moyz.adi.common.service;

import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the GC recount SQL contract: every statement must parse through JSqlParser (the
 * BlockAttackInnerInterceptor does the same at runtime), the scan surface must stay complete
 * (twelve reference sources across draw/message/workflow-runtime/workflow-node/document/kbm
 * tables, live records only) and the workflow extraction must stay structural — jsonb_typeof
 * guards, a string-compared type and URL normalization, never a LIKE-substring approximation.
 * <p>
 * 钉住 GC 复核 SQL 的契约：全部语句可过 JSqlParser（与运行期拦截器同款）；扫描面必须完整
 * （画图/消息/工作流运行/工作流节点/文档/问答死列共十二路引用源，仅统计未删除记录）；
 * 工作流提取必须保持结构化——jsonb_typeof 防御、type 字符串比较与 URL 归一化，禁止 LIKE 子串近似。
 * </p>
 */
class FileGcMapperSqlTest {

    @Test
    void everyXmlStatementParsesThroughJsqlParser() throws Exception {
        for (String template : allStatements()) {
            CCJSqlParserUtil.parse(prepare(template));
        }
    }

    @Test
    void countRefsCoversTheFullScanSurface() throws Exception {
        String sql = prepare(statement("countRefs"));
        assertEquals(11, count(sql, "union all"), "reference source count");
        assertTrue(sql.contains("adi_draw"), "draw table missing");
        assertTrue(sql.contains("adi_character_message"), "message table missing");
        assertTrue(sql.contains("adi_workflow_runtime"), "runtime table missing");
        assertTrue(sql.contains("adi_workflow_runtime_node"), "runtime node table missing");
        assertTrue(sql.contains("adi_document"), "document table missing");
        assertTrue(sql.contains("adi_knowledge_base_message"), "kbm table missing");
        assertTrue(sql.contains("jsonb_array_elements_text"), "array extraction missing");
        assertTrue(sql.contains("string_to_array"), "comma-split extraction missing");
        assertTrue(sql.contains("join adi_file f on f.id = kd.source_file_id"), "source_file_id join missing");
        assertTrue(sql.contains("f.uuid = ANY(") && sql.contains("f.id::text = ANY("),
                "dead-column branch must keep matching both id and uuid formats");
    }

    @Test
    void countRefsCountsLiveRecordsOnly() throws Exception {
        String sql = prepare(statement("countRefs"));
        assertEquals(14, count(sql, "is_deleted = false"), "live-record filters");
    }

    @Test
    void workflowExtractionStaysStructural() throws Exception {
        String sql = prepare(statement("countRefs"));
        assertEquals(9, count(sql, "jsonb_typeof"), "jsonb_typeof guards (4 inner value guards + 4 top-level object guards + 1 draw array guard)");
        assertEquals(4, count(sql, "->> 'type' = '4'"), "string-compared type checks");
        assertEquals(8, count(sql, "regexp_replace"), "URL normalizations");
        assertEquals(2, count(sql, "exists (select 1 from adi_workflow_runtime"), "node-to-runtime joins");
        assertEquals(6, count(sql, "\"output\""), "quoted output columns (extraction + null guard + object guard per runtime row)");
        assertTrue(!sql.contains("like '%"), "LIKE-substring approximation must not sneak in");
    }

    @Test
    void rowTransitionsStayKeyedByPrimaryId() throws Exception {
        assertTrue(prepare(statement("softDeleteById")).contains("where id = ? and is_deleted = false"));
        assertTrue(prepare(statement("updateRefCount")).contains("where id = ? and is_deleted = false"));
        assertTrue(prepare(statement("reviveById")).contains("where id = ? and is_deleted = true"));
        assertTrue(prepare(statement("deleteById")).contains("where id = ?"));
    }

    /**
     * The 017 migration rebuilds ref_count with the same reference surface as countRefs; both halves
     * have drifted independently before. This parses every 017 statement through JSqlParser the same
     * way and pins its surface, so a future edit on either side trips a test. The file is located by
     * probing the two plausible working directories; when found in neither the test is skipped.
     * <p>
     * 017 迁移与 countRefs 用同一套引用扫描面重建计数，此前曾两处独立漂移。本测试以同款方式解析
     * 017 的全部语句并钉住其扫描面，任一侧将来被改都会触发测试。文件按两个候选工作目录探测，
     * 都未找到时跳过。
     * </p>
     */
    @Test
    void migration017KeepsTheSameReferenceSurface() throws Exception {
        java.nio.file.Path file = locate017();
        org.junit.jupiter.api.Assumptions.assumeTrue(null != file, "017_file_gc.sql not reachable from this working directory");
        String sql = java.nio.file.Files.readString(file);
        org.junit.jupiter.api.Assumptions.assumeTrue(count(sql, "\nWITH refs AS (") == 2, "unexpected 017 layout");
        String prepared = sql
                .replaceAll("--[^\n]*", " ")
                .replaceAll("\\s+", " ")
                .trim();
        for (String statement : prepared.split(";")) {
            if (!statement.isBlank()) {
                CCJSqlParserUtil.parse(statement.trim());
            }
        }
        assertEquals(22, count(sql.toLowerCase(), "union all"), "two refs blocks of eleven branches each");
        assertTrue(sql.contains("f.uuid = ANY(string_to_array(kbm.source_file_ids") && sql.contains("f.id::text = ANY(string_to_array(kbm.source_file_ids"),
                "017 dead-column branch must match both formats like the mapper");
        assertTrue(sql.contains("WHERE ref_count <> 0 AND uuid NOT IN (SELECT file_uuid FROM refs WHERE file_uuid IS NOT NULL)"),
                "NULL-safe NOT IN guard missing");
        assertTrue(sql.contains("'file_gc_enabled'") && sql.contains("WHERE NOT EXISTS"), "switch seed insert missing");
        assertTrue(sql.toLowerCase().contains("create unique index if not exists uk_file_uuid"), "uuid index missing");
    }

    private static java.nio.file.Path locate017() {
        for (String candidate : new String[]{"db_migration/017_file_gc.sql", "../db_migration/017_file_gc.sql"}) {
            java.nio.file.Path path = java.nio.file.Path.of(candidate);
            if (java.nio.file.Files.exists(path)) {
                return path;
            }
        }
        return null;
    }

    private static List<String> allStatements() throws Exception {
        List<String> statements = new ArrayList<>();
        try (InputStream in = FileGcMapperSqlTest.class.getClassLoader().getResourceAsStream("mapper/FileGcMapper.xml")) {
            assertNotNull(in, "mapper XML not on classpath");
            var doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            var nodes = doc.getDocumentElement().getElementsByTagName("*");
            for (int i = 0; i < nodes.getLength(); i++) {
                Element e = (Element) nodes.item(i);
                String tag = e.getTagName();
                if ("select".equals(tag) || "update".equals(tag) || "delete".equals(tag) || "insert".equals(tag)) {
                    statements.add(e.getTextContent());
                }
            }
        }
        assertTrue(statements.size() >= 7, "expected all gc statements, got " + statements.size());
        return statements;
    }

    private static String statement(String id) throws Exception {
        try (InputStream in = FileGcMapperSqlTest.class.getClassLoader().getResourceAsStream("mapper/FileGcMapper.xml")) {
            assertNotNull(in, "mapper XML not on classpath");
            var doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            var nodes = doc.getDocumentElement().getElementsByTagName("*");
            for (int i = 0; i < nodes.getLength(); i++) {
                Element e = (Element) nodes.item(i);
                if (id.equals(e.getAttribute("id"))) {
                    return e.getTextContent();
                }
            }
        }
        throw new IllegalStateException("statement not found: " + id);
    }

    private static String prepare(String template) {
        return template
                .replaceAll("#\\{[^}]+}", "?")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replaceAll("\\s+", " ")
                .trim();
    }

    private static int count(String haystack, String needle) {
        int idx = 0, count = 0;
        while ((idx = haystack.indexOf(needle, idx)) >= 0) {
            count++;
            idx += needle.length();
        }
        return count;
    }
}
