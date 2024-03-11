package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.KbEditReq;
import com.moyz.adi.common.dto.QAReq;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.util.BizPager;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.parser.TextDocumentParser;
import dev.langchain4j.data.document.parser.apache.pdfbox.ApachePdfBoxDocumentParser;
import dev.langchain4j.data.document.parser.apache.poi.ApachePoiDocumentParser;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.model.output.Response;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.text.MessageFormat;
import java.time.LocalDateTime;
import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.enums.ErrorEnum.*;
import static dev.langchain4j.data.document.loader.FileSystemDocumentLoader.loadDocument;

@Slf4j
@Service
public class KnowledgeBaseService extends ServiceImpl<KnowledgeBaseMapper, KnowledgeBase> {

    @Lazy
    @Resource
    private KnowledgeBaseService _this;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private RAGService ragService;

    @Resource
    private KnowledgeBaseItemService knowledgeBaseItemService;

    @Resource
    private KnowledgeBaseQaRecordService knowledgeBaseQaRecordService;

    @Resource
    private FileService fileService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;

    @Resource
    private UserDayCostService userDayCostService;

    public KnowledgeBase saveOrUpdate(KbEditReq kbEditReq) {
        String uuid = kbEditReq.getUuid();
        KnowledgeBase knowledgeBase = new KnowledgeBase();
        knowledgeBase.setTitle(kbEditReq.getTitle());
        knowledgeBase.setRemark(kbEditReq.getRemark());
        if (null != kbEditReq.getIsPublic()) {
            knowledgeBase.setIsPublic(kbEditReq.getIsPublic());
        }
        if (null == kbEditReq.getId() || kbEditReq.getId() < 1) {
            User user = ThreadContext.getCurrentUser();
            uuid = UUID.randomUUID().toString().replace("-", "");
            knowledgeBase.setUuid(uuid);
            knowledgeBase.setOwnerId(user.getId());
            knowledgeBase.setOwnerName(user.getName());
            baseMapper.insert(knowledgeBase);
        } else {
            checkPrivilege(kbEditReq.getId(), null);
            knowledgeBase.setId(kbEditReq.getId());
            baseMapper.updateById(knowledgeBase);
        }
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getUuid, uuid)
                .one();
    }

    public List<AdiFile> uploadDocs(String kbUuid, Boolean embedding, MultipartFile[] docs) {
        if (ArrayUtils.isEmpty(docs)) {
            return Collections.emptyList();
        }
        checkPrivilege(null, kbUuid);
        List<AdiFile> result = new ArrayList<>();
        KnowledgeBase knowledgeBase = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getUuid, kbUuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_DATA_NOT_FOUND));
        for (MultipartFile doc : docs) {
            try {
                result.add(uploadDoc(knowledgeBase, doc, embedding));
            } catch (Exception e) {
                log.warn("uploadDocs fail,fileName:{}", doc.getOriginalFilename(), e);
            }
        }
        return result;
    }

    public AdiFile uploadDoc(String kbUuid, Boolean embedding, MultipartFile doc) {
        KnowledgeBase knowledgeBase = ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getUuid, kbUuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .oneOpt()
                .orElseThrow(() -> new BaseException(A_DATA_NOT_FOUND));
        return uploadDoc(knowledgeBase, doc, embedding);
    }

    private AdiFile uploadDoc(KnowledgeBase knowledgeBase, MultipartFile doc, Boolean embedding) {
        try {
            String fileName = doc.getOriginalFilename();
            AdiFile adiFile = fileService.writeToLocal(doc);

            //解析文档
            Document document;
            if (adiFile.getExt().equalsIgnoreCase("txt")) {
                document = loadDocument(adiFile.getPath(), new TextDocumentParser());
            } else if (adiFile.getExt().equalsIgnoreCase("pdf")) {
                document = loadDocument(adiFile.getPath(), new ApachePdfBoxDocumentParser());
            } else if (ArrayUtils.contains(POI_DOC_TYPES, adiFile.getExt())) {
                document = loadDocument(adiFile.getPath(), new ApachePoiDocumentParser());
            } else {
                log.warn("该文件类型:{}无法解析，忽略", adiFile.getExt());
                return adiFile;
            }
            //创建知识库条目
            String uuid = UUID.randomUUID().toString().replace("-", "");
            KnowledgeBaseItem knowledgeBaseItem = new KnowledgeBaseItem();
            knowledgeBaseItem.setUuid(uuid);
            knowledgeBaseItem.setKbId(knowledgeBase.getId());
            knowledgeBaseItem.setKbUuid(knowledgeBase.getUuid());
            knowledgeBaseItem.setSourceFileId(adiFile.getId());
            knowledgeBaseItem.setTitle(fileName);
            knowledgeBaseItem.setBrief(StringUtils.substring(document.text(), 0, 200));
            knowledgeBaseItem.setRemark(document.text());
            knowledgeBaseItem.setIsEmbedded(true);
            boolean success = knowledgeBaseItemService.save(knowledgeBaseItem);
            if (success && Boolean.TRUE.equals(embedding)) {
                knowledgeBaseItem = knowledgeBaseItemService.getEnable(uuid);

                //向量化
                Document docWithoutPath = new Document(document.text());
                docWithoutPath.metadata()
                        .add("kb_uuid", knowledgeBase.getUuid())
                        .add("kb_item_uuid", knowledgeBaseItem.getUuid());

                ragService.ingest(docWithoutPath);

                knowledgeBaseItemService
                        .lambdaUpdate()
                        .eq(KnowledgeBaseItem::getId, knowledgeBaseItem.getId())
                        .set(KnowledgeBaseItem::getIsEmbedded, true)
                        .update();
            }
            return adiFile;
        } catch (Exception e) {
            log.error("upload error", e);
            throw new BaseException(A_UPLOAD_FAIL);
        }
    }

    public boolean embedding(String kbUuid, boolean forceAll) {
        checkPrivilege(null, kbUuid);
        LambdaQueryWrapper<KnowledgeBaseItem> wrapper = new LambdaQueryWrapper();
        wrapper.eq(KnowledgeBaseItem::getIsDeleted, false);
        wrapper.eq(KnowledgeBaseItem::getUuid, kbUuid);
        BizPager.oneByOneWithAnchor(wrapper, knowledgeBaseItemService, KnowledgeBaseItem::getId, one -> {
            if (forceAll || !one.getIsEmbedded()) {
                knowledgeBaseItemService.embedding(one);
            }
        });
        return true;
    }

    public Page<KnowledgeBase> search(String keyword, Boolean includeOthersPublic, Integer currentPage, Integer pageSize) {
        User user = ThreadContext.getCurrentUser();
        if (user.getIsAdmin()) {
            return baseMapper.searchByAdmin(new Page<>(currentPage, pageSize), keyword);
        } else {
            return baseMapper.searchByUser(new Page<>(currentPage, pageSize), user.getId(), keyword, includeOthersPublic);
        }
    }

    public boolean softDelete(String uuid) {
        checkPrivilege(null, uuid);
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBase::getUuid, uuid)
                .set(KnowledgeBase::getIsDeleted, true)
                .update();
    }

    public KnowledgeBaseQaRecord ask(String kbUuid, String question, String modelName) {
        checkRequestTimesOrThrow();
        KnowledgeBase knowledgeBase = getOrThrow(kbUuid);
        Pair<String, Response<AiMessage>> responsePair = ragService.retrieveAndAsk(kbUuid, question, modelName);

        Response<AiMessage> ar = responsePair.getRight();
        int inputTokenCount = ar.tokenUsage().inputTokenCount();
        int outputTokenCount = ar.tokenUsage().outputTokenCount();
        userDayCostService.appendCostToUser(ThreadContext.getCurrentUser(), inputTokenCount + outputTokenCount);
        return knowledgeBaseQaRecordService.createNewRecord(ThreadContext.getCurrentUser(), knowledgeBase, question, responsePair.getLeft(), inputTokenCount, ar.content().text(), outputTokenCount);
    }

    public SseEmitter sseAsk(String kbUuid, QAReq req) {
        checkRequestTimesOrThrow();
        SseEmitter sseEmitter = new SseEmitter();
        _this.retrieveAndPushToLLM(ThreadContext.getCurrentUser(), sseEmitter, kbUuid, req);
        return sseEmitter;
    }

    /**
     * 知识库问答限额判断
     */
    private void checkRequestTimesOrThrow() {
        String key = MessageFormat.format(RedisKeyConstant.AQ_ASK_TIMES, ThreadContext.getCurrentUserId(), LocalDateTimeUtil.format(LocalDateTime.now(), "yyyyMMdd"));
        String askTimes = stringRedisTemplate.opsForValue().get(key);
        String askQuota = SysConfigService.getByKey("quota_by_qa_ask_daily");
        if (null != askTimes && null != askQuota && Integer.parseInt(askTimes) >= Integer.parseInt(askQuota)) {
            throw new BaseException(A_QA_ASK_LIMIT);
        }
        stringRedisTemplate.opsForValue().increment(key);
    }


    @Async
    public void retrieveAndPushToLLM(User user, SseEmitter sseEmitter, String kbUuid, QAReq req) {
        log.info("retrieveAndPushToLLM,kbUuid:{},userId:{}", kbUuid, user.getId());
        KnowledgeBase knowledgeBase = getOrThrow(kbUuid);

        String prompt = ragService.retrieveAndCreatePrompt(kbUuid, req.getQuestion()).text();
        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setSystemMessage(StringUtils.EMPTY);
        sseAskParams.setSseEmitter(sseEmitter);
        sseAskParams.setUserMessage(prompt);
        sseAskParams.setModelName(req.getModelName());
        sseEmitterHelper.process(user, sseAskParams, (response, promptMeta, answerMeta) -> {
            knowledgeBaseQaRecordService.createNewRecord(user, knowledgeBase, req.getQuestion(), prompt, promptMeta.getTokens(), response, answerMeta.getTokens());
            userDayCostService.appendCostToUser(user, promptMeta.getTokens() + answerMeta.getTokens());
        });
    }

    public KnowledgeBase getOrThrow(String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getUuid, kbUuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .oneOpt().orElseThrow(() -> new BaseException(A_DATA_NOT_FOUND));
    }

    private void checkPrivilege(Long kbId, String kbUuid) {
        if (null == kbId && StringUtils.isBlank(kbUuid)) {
            throw new BaseException(A_PARAMS_ERROR);
        }
        User user = ThreadContext.getCurrentUser();
        if (null == user) {
            throw new BaseException(A_USER_NOT_EXIST);
        }
        boolean privilege = user.getIsAdmin();
        if (privilege) {
            return;
        }
        LambdaQueryWrapper<KnowledgeBase> wrapper = new LambdaQueryWrapper();
        wrapper.eq(KnowledgeBase::getOwnerId, user.getId());
        if (null != kbId) {
            wrapper = wrapper.eq(KnowledgeBase::getId, kbId);
        } else if (StringUtils.isNotBlank(kbUuid)) {
            wrapper = wrapper.eq(KnowledgeBase::getUuid, kbUuid);
        }
        boolean exists = baseMapper.exists(wrapper);
        if (!exists) {
            throw new BaseException(A_USER_NOT_AUTH);
        }
    }

}
