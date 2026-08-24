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
import jakarta.annotation.Resource;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.moyz.adi.common.enums.ErrorEnum.A_PARAMS_ERROR;
import static com.moyz.adi.common.enums.ErrorEnum.A_UPLOAD_FAIL;

/**
 * 问答模式数据流：批量导入（Dify 格式 xlsx/csv）与 LLM 自动生成 QA 对。
 * <p>
 * 导入格式与 Dify 对齐：首行为表头 question,answer（容错中文表头"问题/答案"及大小写），
 * 其后每行一对问答；相同答案文本的行自动合并为一个答案段挂多个问题。
 */
@Slf4j
@Service
public class DocumentQaService {

    /**
     * QA 批量导入模板（Dify 格式）
     */
    public static final String QA_IMPORT_TEMPLATE_CSV = "question,answer\n"
            + "What is the capital of France?,The capital of France is Paris.\n"
            + "法国的首都是哪里?,法国的首都是巴黎。\n";

    /**
     * LLM 生成 QA 对的提示词：要求严格输出 JSON 数组
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

    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 解析 QA 文件并落库（不向量化；向量化走索引流程）
     *
     * @return 创建的 qa 模式文档
     */
    public KbDocument importQa(KnowledgeBase kb, String fileName, MultipartFile file) {
        List<QaPair> pairs = parseQaFile(fileName, file);
        if (pairs.isEmpty()) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        // remark 存原始导入文本（Q/A 逐行），便于溯源与重新导入
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
     * 把问答对物化为段行：相同答案文本合并为一个答案段挂多个问题
     */
    public void saveQaPairs(KnowledgeBase kb, KbDocument doc, List<QaPair> pairs, String source) {
        Map<String, List<String>> answerToQuestions = new LinkedHashMap<>();
        for (QaPair pair : pairs) {
            if (StringUtils.isBlank(pair.question()) || StringUtils.isBlank(pair.answer())) {
                continue;
            }
            answerToQuestions.computeIfAbsent(pair.answer().trim(), k -> new ArrayList<>()).add(pair.question().trim());
        }
        int answerPosition = documentSegmentService.listByDocUuid(doc.getUuid()).size();
        for (Map.Entry<String, List<String>> entry : answerToQuestions.entrySet()) {
            DocumentSegment answer = new DocumentSegment();
            answer.setUuid(UuidUtil.createShort());
            answer.setKbUuid(kb.getUuid());
            answer.setDocUuid(doc.getUuid());
            answer.setPosition(answerPosition++);
            answer.setContent(entry.getKey());
            answer.setHitCount(0);
            answer.setSource(source);
            documentSegmentService.save(answer);

            List<DocumentSegmentQuestion> questions = new ArrayList<>();
            int questionPosition = 0;
            for (String questionText : entry.getValue()) {
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
            questionService.saveBatch(questions);
        }
    }

    /**
     * 编辑保存触发的 QA 自动生成入口（同步）：仅对无段行的 qa 模式文档生效（幂等守卫，
     * 不覆盖已有问答数据）；标记生成中（列表立即可见）后派发异步任务
     */
    public void generateQaFromEdit(User user, KbDocument doc) {
        if (doc.getSegmentMode() != com.moyz.adi.common.enums.SegmentModeEnum.QA) {
            return;
        }
        if (!documentSegmentService.listByDocUuid(doc.getUuid()).isEmpty()) {
            return;
        }
        KnowledgeBase kb = knowledgeBaseMapper.selectOne(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<KnowledgeBase>()
                .eq(KnowledgeBase::getUuid, doc.getKbUuid())
                .eq(KnowledgeBase::getIsDeleted, false));
        if (kb == null) {
            return;
        }
        ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                .eq(KbDocument::getUuid, doc.getUuid())
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.DOING)
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getFailReason, null)
                .update();
        generateQaAsync(user, kb, doc);
    }

    /**
     * LLM 自动生成 QA 对（异步）：对已转为 qa 模式的文档分块请求 ingest 模型，
     * 解析 JSON 对后落库、向量化问题并条件落定状态（版本前进则交给新版本流程）
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
                    .set(KbDocument::getFailReason, null)
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
     * 生成失败落定：仅当版本仍一致时生效（期间内容/模式再变更，状态归新版本流程管）
     */
    private void markQaFailed(KbDocument doc, int versionSnapshot, String reason) {
        ChainWrappers.lambdaUpdateChain(kbDocumentService.getBaseMapper())
                .eq(KbDocument::getId, doc.getId())
                .eq(KbDocument::getIndexVersion, versionSnapshot)
                .set(KbDocument::getEmbeddingStatus, EmbeddingStatusEnum.FAIL)
                .set(KbDocument::getEmbeddingStatusChangeTime, java.time.LocalDateTime.now())
                .set(KbDocument::getFailReason, StringUtils.abbreviate(reason, 500))
                .update();
    }

    /**
     * 解析 LLM 输出的 JSON 数组（容错 markdown 代码块包裹）
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
     * 解析 QA 导入文件（Dify 格式）：xlsx 读取前两列；csv 按逗号分割（支持引号包裹字段）
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

    private List<QaPair> parseXlsx(MultipartFile file) throws Exception {
        List<QaPair> pairs = new ArrayList<>();
        try (Workbook workbook = WorkbookFactory.create(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0);
            boolean firstRow = true;
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
                if (StringUtils.isNotBlank(col0) && StringUtils.isNotBlank(col1)) {
                    pairs.add(new QaPair(col0.trim(), col1.trim()));
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
            while ((line = reader.readLine()) != null) {
                if (firstLine) {
                    firstLine = false;
                    // 去 BOM
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
                if (StringUtils.isNotBlank(q) && StringUtils.isNotBlank(a)) {
                    pairs.add(new QaPair(q.trim(), a.trim()));
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
     * csv 行分割：支持双引号包裹（内含逗号/转义引号）
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
            // 避免数字被渲染成科学计数法
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
     * 问答对
     */
    public record QaPair(String question, String answer) {
    }
}
