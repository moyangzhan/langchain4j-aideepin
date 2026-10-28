package com.moyz.adi.common.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * fail_reason 到 user_msg 编码串的转换规则表：阶段前缀剥离、带模型名的分类映射、文件解析、多行堆栈兜底截断。
 * 编码串与 locale 无关，渲染发生在读取侧（i18n bundle）。
 */
class FailReasonConverterTest {

    @Test
    void modelIoFailureBecomesModelCallCode() {
        assertEquals("INDEX_FAIL_MODEL_CALL|THUDM/GLM-Z1-9B-0414",
                FailReasonConverter.toCode("graph: java.io.IOException: EOF reached while reading, name: THUDM/GLM-Z1-9B-0414"));
        assertEquals("INDEX_FAIL_MODEL_CALL|text-embedding-3-small",
                FailReasonConverter.toCode("vectorize: connect timed out, name: text-embedding-3-small"));
    }

    @Test
    void modelAuthRateLimitAndBadResponseAreRecognized() {
        assertEquals("INDEX_FAIL_MODEL_AUTH|gpt-4o",
                FailReasonConverter.toCode("401 Unauthorized, name: gpt-4o"));
        assertEquals("INDEX_FAIL_MODEL_RATE_LIMIT|m",
                FailReasonConverter.toCode("429 Too Many Requests, name: m"));
        assertEquals("INDEX_FAIL_MODEL_BAD_RESPONSE|m",
                FailReasonConverter.toCode("Unexpected character ('}' ...): JSON parse error, name: m"));
    }

    @Test
    void fileParsingIsRecognizedWithoutModel() {
        assertEquals("INDEX_FAIL_FILE_PARSE",
                FailReasonConverter.toCode("PdfException: unsupported encrypted pdf"));
    }

    @Test
    void unknownReasonFallsBackToRawFirstLineTruncated() {
        assertEquals("RAW|java.lang.IllegalArgumentException: header value",
                FailReasonConverter.toCode("java.lang.IllegalArgumentException: header value\n\tat org.example.Foo.bar(Foo.java:1)"));
        assertEquals("RAW|", FailReasonConverter.toCode("  "));
    }
}
