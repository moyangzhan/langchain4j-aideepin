package com.moyz.adi.common.service;

import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the document search SQL contract: the knowledge base scope and the keyword filter are
 * both optional conditions, and every row carries its knowledge base title through a join so
 * cross-base listings stay attributable.
 * <p>
 * 钉住文档搜索 SQL 契约：知识库范围与关键词均为可选条件；每行通过联表带出所属知识库标题，
 * 跨库列表保持可归因。
 * </p>
 */
class KbDocumentMapperSqlTest {

    @Test
    void searchByKbParsesWithAllConditions() throws Exception {
        CCJSqlParserUtil.parse(render(true, true));
    }

    @Test
    void searchByKbParsesWithoutKbUuid() throws Exception {
        CCJSqlParserUtil.parse(render(false, true));
    }

    @Test
    void searchByKbKeepsScopeAndKeywordOptional() throws Exception {
        String template = statement();
        assertTrue(template.contains("<if test=\"kbUuid!='' and kbUuid!=null\">"),
                "kbUuid condition must stay optional");
        assertTrue(template.contains("<if test=\"keyword!='' and keyword!=null\">"),
                "keyword condition must stay optional");
        String withBoth = render(true, true);
        String withoutBoth = render(false, false);
        assertTrue(withBoth.contains("and a.kb_uuid = ?") && withBoth.contains("and a.title like CONCAT('%', ?, '%')"),
                "rendered SQL must keep both filters");
        assertTrue(!withoutBoth.contains("kb_uuid = ?") && !withoutBoth.contains("title like"),
                "dropped conditions must not leak into the rendered SQL");
    }

    @Test
    void searchByKbJoinsKnowledgeBaseTitle() throws Exception {
        String sql = render(false, false);
        assertTrue(sql.contains("left join adi_knowledge_base c on a.kb_id = c.id"),
                "knowledge base join missing");
        assertTrue(sql.contains("c.title kb_title"), "kb_title projection missing");
        assertTrue(sql.contains("where a.is_deleted = false"), "live-record filter missing");
    }

    /**
     * Reads the raw mapper template (tag markup included, unlike DOM text extraction) by probing
     * the two plausible working directories.
     * 按两个候选工作目录探测读取 mapper 原文模板（保留标签结构，区别于 DOM 文本提取）。
     */
    private static String statement() throws Exception {
        Path file = null;
        for (String candidate : new String[]{
                "src/main/resources/mapper/KbDocumentMapper.xml",
                "adi-common/src/main/resources/mapper/KbDocumentMapper.xml"}) {
            Path path = Path.of(candidate);
            if (Files.exists(path)) {
                file = path;
                break;
            }
        }
        assertNotNull(file, "mapper XML not reachable from this working directory");
        Matcher matcher = Pattern
                .compile("<select id=\"searchByKb\"[^>]*>([\\s\\S]*?)</select>")
                .matcher(Files.readString(file));
        assertTrue(matcher.find(), "statement not found: searchByKb");
        return matcher.group(1);
    }

    /**
     * Renders the template the way MyBatis would for the given parameter presence: an <if>
     * block survives only when its test names a present parameter.
     * 按参数是否存在渲染模板：只有测试条件命中的 <if> 块会保留。
     */
    private static String render(boolean kbUuidPresent, boolean keywordPresent) throws Exception {
        String template = statement();
        String out = template;
        int open;
        while ((open = out.indexOf("<if")) >= 0) {
            int testEnd = out.indexOf('>', open);
            int close = out.indexOf("</if>", open);
            String test = out.substring(open, testEnd);
            String body = out.substring(testEnd + 1, close);
            boolean keep = (test.contains("kbUuid") && kbUuidPresent) || (test.contains("keyword") && keywordPresent);
            String replacement = keep ? body : "";
            out = out.substring(0, open) + replacement + out.substring(close + "</if>".length());
        }
        return out
                .replaceAll("#\\{[^}]+}", "?")
                .replaceAll("\\s+", " ")
                .trim();
    }
}
