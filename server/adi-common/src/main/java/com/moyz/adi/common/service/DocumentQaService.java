package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.entity.DocumentSegment;
import com.moyz.adi.common.entity.DocumentSegmentQuestion;
import com.moyz.adi.common.entity.KbDocument;
import com.moyz.adi.common.entity.KnowledgeBase;
import com.moyz.adi.common.entity.User;
import com.moyz.adi.common.enums.EmbeddingStatusEnum;
import com.moyz.adi.common.enums.GraphicalStatusEnum;
import com.moyz.adi.common.util.AdiStringUtil;
import com.moyz.adi.common.enums.LLMCallRecordSourceType;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.LLMContext;
import com.moyz.adi.common.languagemodel.AbstractLLMService;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.rag.DocumentSplitterFactory;
import com.moyz.adi.common.rag.TokenEstimatorFactory;
import com.moyz.adi.common.util.UuidUtil;
import com.moyz.adi.common.vo.ChatModelBuilderProperties;
import dev.langchain4j.data.document.DefaultDocument;
import dev.langchain4j.data.document.DocumentSplitter;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.model.chat.ChatModel;
import dev.langchain4j.model.chat.response.ChatResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.context.annotation.Lazy;
import jakarta.annotation.Resource;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.A_DATA_NOT_FOUND;
import static com.moyz.adi.common.enums.ErrorEnum.A_DOC_INDEX_DOING;
import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_UPLOAD_FAIL;

/**
 * QA-mode data flows: bulk import (Dify-format xlsx/csv) and LLM auto generation of QA pairs.
 * <p>
 * The import format aligns with Dify: the first row is the header question,answer (tolerating
 * the Chinese headers 问题/答案 and case differences), each following row is one pair; rows with
 * identical answer text are merged into one answer segment holding multiple questions.
 */
@Slf4j
@Service
public class DocumentQaService {

    /**
     * QA bulk-import template: one question per cell, never split by newlines; a row with an
     * empty answer continues the previous non-empty answer (multiple questions per answer —
     * vertically merged answer cells in Excel read out the same way), and rows with identical
     * answer text are also merged into one answer with multiple questions
     */
    public static final String QA_IMPORT_TEMPLATE_CSV = "question,answer\n"
            + "What is the capital of France?,The capital of France is Paris.\n"
            + "Which city is the capital of France?,\n"
            + "法国的首都是哪里?,法国的首都是巴黎。\n"
            + "法国的首都是什么城市?,\n";

    /**
     * Prompt for LLM QA generation: requires a strict JSON array output
     */
    public static final String QA_GENERATE_PROMPT = "You are a data annotation expert. Read the text below and generate"
            + " high-quality question-answer pairs based on it. Each question should be answerable from the text alone,"
            + " and each answer should be concise and self-contained.\n"
            + "Output STRICTLY a JSON array and nothing else (no markdown fences, no explanations),"
            + " in the format: [{\"question\":\"...\",\"answer\":\"...\"}]\n"
            + "Generate at most 5 pairs.\n"
            + "Text:\n{input_text}";

    private static final int GENERATE_CHUNK_SIZE = 1500;

    @Resource
    private DocumentSegmentService documentSegmentService;

    @Resource
    private DocumentSegmentQuestionService questionService;

    @Resource
    private SegmentIndexService segmentIndexService;

    @Resource
    private KbDocumentService kbDocumentService;

    @Resource
    private LLMCallRecordService llmCallRecordService;

    @Resource
    private ModelHealthService modelHealthService;

    @Resource
    private KnowledgeBaseMapper knowledgeBaseMapper;

    // @Lazy: indexTaskService's dependency graph may point back at this service's callers
    // (KbDocumentService); lazy injection breaks the cycle
    @Lazy
    @Resource
    private IndexTaskService indexTaskService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Parse the QA file and persist it (no vectorization here; that goes through the index flow)
     *
     * @return the created qa-mode document
     */
    public KbDocument importQa(KnowledgeBase kb, String fileName, MultipartFile file) {
        List<QaPair> pairs = parseQaFile(fileName, file);
        if (pairs.isEmpty()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        // remark keeps the raw imported text (Q/A per pair) for traceability and re-import
        String remark = pairs.stream()
                .map(pair -> "Q: " + pair.question() + "\nA: " + pair.answer())
                .collect(Collectors.joining("\n\n"));

        KbDocument doc = new KbDocument();
        doc.setUuid(UuidUtil.createShort());
        doc.setKbId(kb.getId());
        doc.setKbUuid(kb.getUuid());
        doc.setTitle(fileName);
        doc.setBrief(StringUtils.substring(remark, 0, 200));
        doc.setRemark(remark);
        doc.setSegmentMode(com.moyz.adi.common.enums.SegmentModeEnum.QA);
        kbDocumentService.save(doc);

        saveQaPairs(kb, doc, pairs, AdiConstant.SegmentSource.DOC);
        return doc;
    }

    /**
     * Materialize QA pairs into segment rows: identical answer text is merged into one answer
     * segment holding multiple questions; an answer matching an existing segment's content joins
     * that segment (question texts deduplicated), and only new answers create segment rows —
     * shared by creation-time import and append import
     */
    public void saveQaPairs(KnowledgeBase kb, KbDocument doc, List<QaPair> pairs, String source) {
        Map<String, List<String>> answerToQuestions = new LinkedHashMap<>();
        for (QaPair pair : pairs) {
            if (StringUtils.isBlank(pair.question()) || StringUtils.isBlank(pair.answer())) {
                continue;
            }
            // One cell = one question; embedded newlines are collapsed to spaces, never split
            // (users cannot tell whether copied text contains newlines, and line-splitting would
            // silently corrupt the question) — normalization guarantees "stored questions never contain newlines"
            String question = AdiStringUtil.normalizeSingleLine(pair.question());
            if (question.isEmpty()) {
                continue;
            }
            answerToQuestions.computeIfAbsent(pair.answer().trim(), k -> new ArrayList<>()).add(question);
        }
        Map<String, DocumentSegment> existingAnswers = documentSegmentService.listByDocUuid(doc.getUuid()).stream()
                .collect(Collectors.toMap(DocumentSegment::getContent, s -> s, (a, b) -> a));
        int answerPosition = existingAnswers.size();
        for (Map.Entry<String, List<String>> entry : answerToQuestions.entrySet()) {
            DocumentSegment answer = existingAnswers.get(entry.getKey());
            if (answer == null) {
                answer = new DocumentSegment();
                answer.setUuid(UuidUtil.createShort());
                answer.setKbUuid(kb.getUuid());
                answer.setDocUuid(doc.getUuid());
                answer.setPosition(answerPosition++);
                answer.setContent(entry.getKey());
                answer.setHitCount(0);
                answer.setSource(source);
                documentSegmentService.save(answer);
            }
            // Joining an existing answer / in-batch duplicates: skip question texts that already
            // exist to avoid duplicate vectorization
            Set<String> existingQuestionTexts = questionService.listByAnswerIds(List.of(answer.getId())).stream()
                    .map(DocumentSegmentQuestion::getContent)
                    .collect(Collectors.toSet());
            List<DocumentSegmentQuestion> questions = new ArrayList<>();
            int questionPosition = existingQuestionTexts.size();
            for (String questionText : entry.getValue()) {
                if (existingQuestionTexts.contains(questionText)) {
                    continue;
                }
                DocumentSegmentQuestion question = new DocumentSegmentQuestion();
                question.setUuid(UuidUtil.createShort());
                question.setKbUuid(kb.getUuid());
                question.setDocUuid(doc.getUuid());
                question.setAnswerSegmentId(answer.getId());
                question.setPosition(questionPosition++);
                question.setContent(questionText);
                question.setHitCount(0);
                questions.add(question);
            }
            if (!questions.isEmpty()) {
                questionService.saveBatch(questions);
            }
        }
    }

    /**
     * Import QA pairs into an existing qa document (append semantics): mutually exclusive with
     * generation / running tasks on the same document; identical answers join existing segments
     * with question-text dedup, then pending questions are vectorized and the status is finalized
     * conditionally on the version (clearing the failure reason)
     */
    public void importQaToDocument(KbDocument doc, MultipartFile file) {
        if (EmbeddingStatusEnum.DOING == doc.getEmbeddingStatus()
                || GraphicalStatusEnum.DOING == doc.getGraphicalStatus()
                || indexTaskService.hasRunningByDoc(doc.getUuid())) {
            throw new BaseException(A_DOC_INDEX_DOING);
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid())
                .eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            throw new BaseException(A_DATA_NOT_FOUND);
        }
        int versionSnapshot = doc.getIndexVersion() == null ? 0 : doc.getIndexVersion();
        String fileName = file.getOriginalFilename();
        List<QaPair> pairs = parseQaFile(fileName == null || fileName.isBlank() ? "qa_import" : fileName, file);
        if (pairs.isEmpty()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        try {
            saveQaPairs(kb, doc, pairs, AdiConstant.SegmentSource.DOC);
            segmentIndexService.vectorizePendingQuestions(kb, doc);
        } catch (Exception e) {
            // Finalize even when vectorization fails: the question rows are already persisted, and
            // a status stuck at its old value (e.g. DONE) would hide "new questions have no vector".
            // Version-conditional FAIL with the import: prefix, same convention as qa_generate/vectorize/graph
            ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                    .eq(KbDocument::getId, doc.getId())
                    .eq(KbDocument::getIndexVersion, versionSnapshot)
                    .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.FAIL)
                    .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                    .set(KbDocument::getFailReason, StringUtils.abbreviate(
                            "import: " + (e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName()), 500))
                    .update();
            throw e;
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, versionSnapshot)
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getFailReason, "")
                .update();
    }

    /**
     * (Re)generate QA pairs: with no segment rows it generates directly; with existing pairs it
     * is a replace-style regeneration — after rejecting generation-in-progress / running tasks on
     * the same document, segment rows / questions / child chunks / vectors are cleared and the
     * document re-fetched (async generation works off the post-cleanup new version), then marked
     * as generating and dispatched as an async task.
     */
    public void autoGenerateQa(User user, KbDocument doc) {
        if (doc.getSegmentMode() != com.moyz.adi.common.enums.SegmentModeEnum.QA) {
            return;
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid())
                .eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            return;
        }
        if (!documentSegmentService.listByDocUuid(doc.getUuid()).isEmpty()) {
            if (EmbeddingStatusEnum.DOING == doc.getEmbeddingStatus()
                    || GraphicalStatusEnum.DOING == doc.getGraphicalStatus()
                    || indexTaskService.hasRunningByDoc(doc.getUuid())) {
                throw new BaseException(A_DOC_INDEX_DOING);
            }
            kbDocumentService.clearSegmentsForQaRegenerate(doc.getUuid());
            doc = kbDocumentService.getEnable(doc.getUuid());
            if (doc == null) {
                return;
            }
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                .eq(KbDocument::getUuid, doc.getUuid())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getFailReason, "")
                .update();
        generateQaAsync(user, kb, doc);
    }

    /**
     * LLM auto QA generation (async): chunks the qa-mode document's text and requests the ingest
     * model per chunk, persists the parsed JSON pairs, vectorizes the questions and finalizes the
     * status conditionally on the version (a version advance hands over to the new-version flow)
     */
    @Async
    public void generateQaAsync(User user, KnowledgeBase kb, KbDocument doc) {
        AbstractLLMService llmService = null;
        int versionSnapshot = doc.getIndexVersion() == null ? 0 : doc.getIndexVersion();
        try {
            llmService = LLMContext.getServiceById(kb.getIngestModelId(), true);
            ChatModel chatModel = llmService.buildChatLLM(ChatModelBuilderProperties.builder()
                    .temperature(kb.getQueryLlmTemperature())
                    .build());

            DocumentSplitter splitter = DocumentSplitterFactory.create(
                    kb.getIngestSplitStrategy(),
                    GENERATE_CHUNK_SIZE,
                    kb.getIngestMaxOverlap(),
                    kb.getIngestCustomSeparator(),
                    TokenEstimatorFactory.create(kb.getIngestTokenEstimator()));
            Metadata metadata = new Metadata();
            metadata.put(AdiConstant.MetadataKey.KB_UUID, kb.getUuid());
            metadata.put(AdiConstant.MetadataKey.KB_ITEM_UUID, doc.getUuid());
            List<TextSegment> chunks = splitter.split(new DefaultDocument(doc.getRemark(), metadata));

            List<QaPair> pairs = new ArrayList<>();
            int totalTokens = 0;
            long startTime = System.currentTimeMillis();
            for (TextSegment chunk : chunks) {
                if (StringUtils.isBlank(chunk.text())) {
                    continue;
                }
                ChatResponse response = chatModel.chat(dev.langchain4j.data.message.UserMessage.from(
                        QA_GENERATE_PROMPT.replace("{input_text}", chunk.text())));
                if (response.tokenUsage() != null) {
                    totalTokens += response.tokenUsage().totalTokenCount();
                }
                pairs.addAll(parseQaJson(response.aiMessage().text()));
            }
            if (pairs.isEmpty()) {
                log.info("generateQa produced no pairs, docUuid:{}", doc.getUuid());
                markQaFailed(doc, versionSnapshot, "no valid QA pairs generated");
                return;
            }
            saveQaPairs(kb, doc, pairs, AdiConstant.SegmentSource.DOC);
            segmentIndexService.vectorizePendingQuestions(kb, doc);
            ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                    .eq(KbDocument::getId, doc.getId())
                    .eq(KbDocument::getIndexVersion, versionSnapshot)
                    .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DONE)
                    .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                    .set(KbDocument::getFailReason, "")
                    .update();
            log.info("generateQa done, docUuid:{}, pairs:{}", doc.getUuid(), pairs.size());

            if (totalTokens > 0 && user != null) {
                com.moyz.adi.common.entity.LLMCallRecord callRecord = new com.moyz.adi.common.entity.LLMCallRecord();
                callRecord.setUuid(UuidUtil.createShort());
                callRecord.setSourceType(LLMCallRecordSourceType.KNOWLEDGE_BASE_INGEST.getValue());
                callRecord.setSourceId(doc.getId());
                callRecord.setUserId(user.getId());
                callRecord.setModelPlatform(llmService.getAiModel().getPlatform());
                callRecord.setModelName(llmService.getAiModel().getName());
                callRecord.setInputTokens(totalTokens);
                callRecord.setOutputTokens(0);
                callRecord.setDuration((int) (System.currentTimeMillis() - startTime));
                llmCallRecordService.saveAsync(callRecord);
            }
        } catch (Exception e) {
            if (null != llmService) {
                modelHealthService.recordFailure(llmService.getAiModel().getName(), e);
            }
            log.error("generateQa error, docUuid:{}", doc.getUuid(), e);
            markQaFailed(doc, versionSnapshot, e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName());
        }
    }

    /**
     * Finalize a generation failure: effective only while the version still matches (if content
     * or mode changed meanwhile, the status belongs to the new-version flow). The qa_generate:
     * prefix marks the failing stage (same write-side convention as vectorize: / graph:)
     */
    private void markQaFailed(KbDocument doc, int versionSnapshot, String reason) {
        ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, versionSnapshot)
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.FAIL)
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getFailReason, StringUtils.abbreviate("qa_generate: " + reason, 500))
                .update();
    }

    /**
     * Parse the JSON array from LLM output (tolerates markdown code-fence wrapping)
     */
    private List<QaPair> parseQaJson(String text) {
        if (StringUtils.isBlank(text)) {
            return List.of();
        }
        String json = text.trim();
        if (json.startsWith("```")) {
            int start = json.indexOf('\n');
            int end = json.lastIndexOf("```");
            if (start >= 0 && end > start) {
                json = json.substring(start + 1, end).trim();
            }
        }
        int arrayStart = json.indexOf('[');
        int arrayEnd = json.lastIndexOf(']');
        if (arrayStart < 0 || arrayEnd <= arrayStart) {
            log.warn("generateQa response is not a JSON array: {}", StringUtils.substring(json, 0, 200));
            return List.of();
        }
        try {
            JsonNode array = objectMapper.readTree(json.substring(arrayStart, arrayEnd + 1));
            List<QaPair> pairs = new ArrayList<>();
            for (JsonNode node : array) {
                String question = node.path("question").asText(null);
                String answer = node.path("answer").asText(null);
                if (StringUtils.isNotBlank(question) && StringUtils.isNotBlank(answer)) {
                    pairs.add(new QaPair(question, answer));
                }
            }
            return pairs;
        } catch (Exception e) {
            log.warn("generateQa JSON parse failed", e);
            return List.of();
        }
    }

    /**
     * Parse the QA import file (Dify format): xlsx reads the first two columns; csv splits on
     * commas (quoted fields supported)
     */
    private List<QaPair> parseQaFile(String fileName, MultipartFile file) {
        String lower = fileName == null ? "" : fileName.toLowerCase();
        try {
            List<QaPair> pairs;
            if (lower.endsWith(".xlsx") || lower.endsWith(".xls")) {
                pairs = parseXlsx(file);
            } else if (lower.endsWith(".csv")) {
                pairs = parseCsv(file);
            } else {
                throw new BaseException(A_PARAMS_ERROR);
            }
            if (pairs.isEmpty()) {
                throw new BaseException(A_PARAMS_ERROR);
            }
            return pairs;
        } catch (BaseException e) {
            throw e;
        } catch (Exception e) {
            log.error("parseQaFile error, fileName:{}", fileName, e);
            throw new BaseException(A_UPLOAD_FAIL);
        }
    }

    /**
     * Two ways to express multiple questions per answer (csv/xlsx alike, one question per cell,
     * never split by newlines):
     * 1. A row with an empty answer continues the previous non-empty answer (the first data row
     *    must have both values); vertically merged answer cells in Excel read out exactly this
     *    shape (only the top-left cell holds a value), so the same logic supports them natively
     * 2. Rows with fully identical answer text are merged by saveQaPairs' answer grouping into
     *    one answer holding multiple questions
     */
    private List<QaPair> parseXlsx(MultipartFile file) throws Exception {
        List<QaPair> pairs = new ArrayList<>();
        try (Workbook workbook = WorkbookFactory.create(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0);
            boolean firstRow = true;
            String lastAnswer = null;
            for (Row row : sheet) {
                String col0 = cellText(row.getCell(0));
                String col1 = cellText(row.getCell(1));
                if (firstRow) {
                    firstRow = false;
                    if (isHeaderRow(col0, col1)) {
                        continue;
                    }
                    throw new BaseException(A_PARAMS_ERROR);
                }
                if (StringUtils.isNotBlank(col1)) {
                    lastAnswer = col1.trim();
                }
                if (StringUtils.isNotBlank(col0) && lastAnswer != null) {
                    pairs.add(new QaPair(col0.trim(), lastAnswer));
                }
            }
        }
        return pairs;
    }

    private List<QaPair> parseCsv(MultipartFile file) throws Exception {
        List<QaPair> pairs = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream(), StandardCharsets.UTF_8))) {
            String line;
            boolean firstLine = true;
            String lastAnswer = null;
            while ((line = reader.readLine()) != null) {
                if (firstLine) {
                    firstLine = false;
                    // strip BOM
                    if (line.startsWith("\uFEFF")) {
                        line = line.substring(1);
                    }
                    if (line.isBlank()) {
                        continue;
                    }
                    List<String> cols = splitCsvLine(line);
                    if (isHeaderRow(col(cols, 0), col(cols, 1))) {
                        continue;
                    }
                    throw new BaseException(A_PARAMS_ERROR);
                }
                List<String> cols = splitCsvLine(line);
                String q = col(cols, 0);
                String a = col(cols, 1);
                if (StringUtils.isNotBlank(a)) {
                    lastAnswer = a.trim();
                }
                if (StringUtils.isNotBlank(q) && lastAnswer != null) {
                    pairs.add(new QaPair(q.trim(), lastAnswer));
                }
            }
        }
        return pairs;
    }

    private boolean isHeaderRow(String col0, String col1) {
        String q = col0 == null ? "" : col0.trim().toLowerCase();
        String a = col1 == null ? "" : col1.trim().toLowerCase();
        return ("question".equals(q) || "问题".equals(q) || "q".equals(q))
                && ("answer".equals(a) || "答案".equals(a) || "a".equals(a));
    }

    /**
     * csv line splitting: supports double-quoted fields (embedded commas / escaped quotes)
     */
    private List<String> splitCsvLine(String line) {
        List<String> result = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (inQuotes) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        current.append('"');
                        i++;
                    } else {
                        inQuotes = false;
                    }
                } else {
                    current.append(c);
                }
            } else if (c == '"') {
                inQuotes = true;
            } else if (c == ',') {
                result.add(current.toString());
                current.setLength(0);
            } else {
                current.append(c);
            }
        }
        result.add(current.toString());
        return result;
    }

    private String col(List<String> cols, int index) {
        return index < cols.size() ? cols.get(index) : null;
    }

    private String cellText(Cell cell) {
        if (cell == null) {
            return "";
        }
        if (cell.getCellType() == CellType.STRING) {
            return cell.getStringCellValue();
        }
        if (cell.getCellType() == CellType.NUMERIC) {
            // keep numbers from rendering in scientific notation
            double d = cell.getNumericCellValue();
            if (d == Math.floor(d) && !Double.isInfinite(d)) {
                return String.valueOf((long) d);
            }
            return String.valueOf(d);
        }
        if (cell.getCellType() == CellType.FORMULA) {
            try {
                return cell.getStringCellValue();
            } catch (Exception e) {
                return String.valueOf(cell.getNumericCellValue());
            }
        }
        return "";
    }

    /**
     * A question-answer pair
     */
    public record QaPair(String question, String answer) {
    }
}
