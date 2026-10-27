package com.moyz.adi.common.workflow;

import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * BlockAttackInnerInterceptor parses EVERY statement through JSqlParser before execution — a
 * reserved word in a hand-written mapper SQL (e.g. OUTPUT) therefore fails at runtime with
 * "Failed to process", not at compile time. This test parses every statement of the runtime
 * mapper's XML the same way (#{...} -> ?, &lt; -> <), so such breakage is caught here instead
 * of on the first live workflow.
 * <p>
 * BlockAttackInnerInterceptor 执行前会经 JSqlParser 解析全部语句，手写 SQL 踩中保留字（如
 * OUTPUT）只会在运行期暴露。本测试以同样方式解析本 mapper XML 的全部语句，把该类问题挡在测试期。
 * </p>
 */
class WorkflowRunMapperSqlTest {

    @Test
    void everyXmlStatementParsesThroughJsqlParser() throws Exception {
        List<String> statements = new ArrayList<>();
        try (InputStream in = getClass().getClassLoader().getResourceAsStream("mapper/WorkflowInstanceMapper.xml")) {
            assertTrue(null != in, "mapper XML not on classpath");
            var doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            var nodes = doc.getDocumentElement().getElementsByTagName("*");
            for (int i = 0; i < nodes.getLength(); i++) {
                Element e = (Element) nodes.item(i);
                if ("select".equals(e.getTagName()) || "update".equals(e.getTagName()) || "delete".equals(e.getTagName()) || "insert".equals(e.getTagName())) {
                    statements.add(e.getTextContent());
                }
            }
        }
        assertTrue(statements.size() >= 5, "expected pageByWfUuid + the checkpoint statements, got " + statements.size());
        for (String sqlTemplate : statements) {
            //Mirror MyBatis here: #{...} becomes ? and &lt; resolves back to < before the interceptor sees the statement
            String prepared = sqlTemplate
                    .replaceAll("#\\{[^}]+}", "?")
                    .replace("&lt;", "<")
                    .replace("&gt;", ">")
                    .replaceAll("\\s+", " ")
                    .trim();
            CCJSqlParserUtil.parse(prepared);
        }
    }
}
