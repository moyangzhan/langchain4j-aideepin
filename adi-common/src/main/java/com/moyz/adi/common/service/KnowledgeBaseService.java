package com.moyz.adi.common.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.ChainWrappers;
import com.moyz.adi.common.base.ThreadContext;
import com.moyz.adi.common.cosntant.AdiConstant;
import com.moyz.adi.common.cosntant.RedisKeyConstant;
import com.moyz.adi.common.dto.KbEditReq;
import com.moyz.adi.common.dto.KbInfoResp;
import com.moyz.adi.common.dto.KbSearchReq;
import com.moyz.adi.common.entity.*;
import com.moyz.adi.common.exception.BaseException;
import com.moyz.adi.common.helper.SSEEmitterHelper;
import com.moyz.adi.common.mapper.KnowledgeBaseMapper;
import com.moyz.adi.common.util.*;
import com.moyz.adi.common.vo.AssistantChatParams;
import com.moyz.adi.common.vo.LLMBuilderProperties;
import com.moyz.adi.common.vo.SseAskParams;
import dev.langchain4j.data.document.Document;
import dev.langchain4j.data.document.parser.TextDocumentParser;
import dev.langchain4j.data.document.parser.apache.pdfbox.ApachePdfBoxDocumentParser;
import dev.langchain4j.data.document.parser.apache.poi.ApachePoiDocumentParser;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.text.MessageFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

import static com.moyz.adi.common.cosntant.AdiConstant.POI_DOC_TYPES;
import static com.moyz.adi.common.cosntant.AdiConstant.SysConfigKey.QUOTA_BY_QA_ASK_DAILY;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.KB_STATISTIC_RECALCULATE_SIGNAL;
import static com.moyz.adi.common.cosntant.RedisKeyConstant.TOKEN_USAGE_KEY;
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
    private KnowledgeBaseStarRecordService knowledgeBaseStarRecordService;

    @Resource
    private FileService fileService;

    @Resource
    private SSEEmitterHelper sseEmitterHelper;

    @Resource
    private UserDayCostService userDayCostService;

    @Resource
    private AiModelService aiModelService;

    public boolean updateKb(KbEditReq kbEditReq) {
        KnowledgeBase existKb = getOrThrow(kbEditReq.getUuid());
        KnowledgeBase knowledgeBase = new KnowledgeBase();
        knowledgeBase.setTitle(kbEditReq.getTitle());
        knowledgeBase.setRemark(kbEditReq.getRemark());
        if (null != kbEditReq.getIsPublic()) {
            knowledgeBase.setIsPublic(kbEditReq.getIsPublic());
        }
        knowledgeBase.setId(existKb.getId());
        knowledgeBase.setRagMaxOverlap(kbEditReq.getRagMaxOverlap());
        knowledgeBase.setRagMaxResults(kbEditReq.getRagMaxResults());
        knowledgeBase.setRagMinScore(kbEditReq.getRagMinScore());
        baseMapper.updateById(knowledgeBase);
        return true;
    }

    public KnowledgeBase saveOrUpdate(KbEditReq kbEditReq) {
        KnowledgeBase knowledgeBase = new KnowledgeBase();
        BeanUtils.copyProperties(kbEditReq, knowledgeBase, "id", "uuid");
        if (null == kbEditReq.getId() || kbEditReq.getId() < 1) {
            User user = ThreadContext.getCurrentUser();
            knowledgeBase.setUuid(UuidUtil.createShort());
            knowledgeBase.setOwnerId(user.getId());
            knowledgeBase.setOwnerUuid(user.getUuid());
            knowledgeBase.setOwnerName(user.getName());
            baseMapper.insert(knowledgeBase);
        } else {
            checkPrivilege(kbEditReq.getId(), null);
            knowledgeBase.setId(kbEditReq.getId());
            baseMapper.updateById(knowledgeBase);
        }
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getId, kbEditReq.getId())
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
            String uuid = UuidUtil.createShort();
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
                        .put(AdiConstant.EmbeddingMetadataKey.KB_UUID, knowledgeBase.getUuid())
                        .put(AdiConstant.EmbeddingMetadataKey.KB_ITEM_UUID, knowledgeBaseItem.getUuid());
                ragService.ingest(docWithoutPath, knowledgeBase.getRagMaxOverlap());

                knowledgeBaseItemService
                        .lambdaUpdate()
                        .eq(KnowledgeBaseItem::getId, knowledgeBaseItem.getId())
                        .set(KnowledgeBaseItem::getIsEmbedded, true)
                        .update();

                updateStatistic(knowledgeBase.getUuid());
            }
            return adiFile;
        } catch (Exception e) {
            log.error("upload error", e);
            throw new BaseException(A_UPLOAD_FAIL);
        }
    }

    public boolean embedding(String kbUuid, boolean forceAll) {
        checkPrivilege(null, kbUuid);
        KnowledgeBase knowledgeBase = this.getOrThrow(kbUuid);
        LambdaQueryWrapper<KnowledgeBaseItem> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(KnowledgeBaseItem::getIsDeleted, false);
        wrapper.eq(KnowledgeBaseItem::getUuid, kbUuid);
        BizPager.oneByOneWithAnchor(wrapper, knowledgeBaseItemService, KnowledgeBaseItem::getId, one -> {
            if (forceAll || Boolean.FALSE.equals(one.getIsEmbedded())) {
                knowledgeBaseItemService.embedding(knowledgeBase, one);
            }
        });
        return true;
    }

    public boolean itemEmbedding(String itemUuid) {
        KnowledgeBase knowledgeBase = baseMapper.getByItemUuid(itemUuid);
        return knowledgeBaseItemService.checkAndEmbedding(knowledgeBase, itemUuid);
    }

    public boolean itemsEmbedding(String[] itemUuids) {
        if (itemUuids.length == 0) {
            return false;
        }
        KnowledgeBase knowledgeBase = baseMapper.getByItemUuid(itemUuids[0]);
        return knowledgeBaseItemService.checkAndEmbedding(knowledgeBase, itemUuids);
    }

    public Page<KbInfoResp> searchMine(String keyword, Boolean includeOthersPublic, Integer currentPage, Integer pageSize) {
        Page<KbInfoResp> result = new Page<>();
        User user = ThreadContext.getCurrentUser();
        Page<KnowledgeBase> knowledgeBasePage;
        if (Boolean.TRUE.equals(user.getIsAdmin())) {
            knowledgeBasePage = baseMapper.searchByAdmin(new Page<>(currentPage, pageSize), keyword);
        } else {
            knowledgeBasePage = baseMapper.searchByUser(new Page<>(currentPage, pageSize), user.getId(), keyword, includeOthersPublic);
        }
        return MPPageUtil.convertToPage(knowledgeBasePage, result, KbInfoResp.class, null);
    }

    public Page<KbInfoResp> search(KbSearchReq req, Integer currentPage, Integer pageSize) {
        Page<KbInfoResp> result = new Page<>();
        LambdaQueryWrapper<KnowledgeBase> wrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(req.getTitle())) {
            wrapper.like(KnowledgeBase::getTitle, req.getTitle());
        }
        if (StringUtils.isNotBlank(req.getOwnerName())) {
            wrapper.like(KnowledgeBase::getOwnerName, req.getOwnerName());
        }
        if (null != req.getIsPublic()) {
            wrapper.eq(KnowledgeBase::getIsPublic, req.getIsPublic());
        }
        if (null != req.getMinItemCount()) {
            wrapper.ge(KnowledgeBase::getItemCount, req.getMinItemCount());
        }
        if (null != req.getMinEmbeddingCount()) {
            wrapper.ge(KnowledgeBase::getEmbeddingCount, req.getMinEmbeddingCount());
        }
        if (null != req.getCreateTime() && req.getCreateTime().length == 2) {
            wrapper.between(KnowledgeBase::getCreateTime, LocalDateTimeUtil.parse(req.getCreateTime()[0]), LocalDateTimeUtil.parse(req.getCreateTime()[1]));
        }
        if (null != req.getUpdateTime() && req.getUpdateTime().length == 2) {
            wrapper.between(KnowledgeBase::getUpdateTime, LocalDateTimeUtil.parse(req.getUpdateTime()[0]), LocalDateTimeUtil.parse(req.getUpdateTime()[1]));
        }
        wrapper.eq(KnowledgeBase::getIsDeleted, false);
        wrapper.orderByDesc(KnowledgeBase::getStarCount, KnowledgeBase::getUpdateTime);
        Page<KnowledgeBase> knowledgeBasePage = baseMapper.selectPage(new Page<>(currentPage, pageSize), wrapper);
        return MPPageUtil.convertToPage(knowledgeBasePage, result, KbInfoResp.class, null);
    }

    public boolean softDelete(String uuid) {
        checkPrivilege(null, uuid);
        return ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBase::getUuid, uuid)
                .set(KnowledgeBase::getIsDeleted, true)
                .update();
    }

    public SseEmitter sseAsk(String qaRecordUuid) {
        checkRequestTimesOrThrow();
        SseEmitter sseEmitter = new SseEmitter();
        User user = ThreadContext.getCurrentUser();
        if (!sseEmitterHelper.checkOrComplete(user, sseEmitter)) {
            return sseEmitter;
        }
        sseEmitterHelper.startSse(user, sseEmitter);
        _this.retrieveAndPushToLLM(user, sseEmitter, qaRecordUuid);
        return sseEmitter;
    }

    /**
     * Star or unstar
     *
     * @param user
     * @param kbUuid
     * @return true:star;false:unstar
     */
    @Transactional
    public boolean toggleStar(User user, String kbUuid) {

        KnowledgeBase knowledgeBase = _this.getOrThrow(kbUuid);
        boolean star;
        KnowledgeBaseStarRecord oldRecord = knowledgeBaseStarRecordService.getRecord(user.getId(), kbUuid);
        if (null == oldRecord) {
            KnowledgeBaseStarRecord starRecord = new KnowledgeBaseStarRecord();
            starRecord.setUserId(user.getId());
            starRecord.setUserUuid(user.getUuid());
            starRecord.setKbId(knowledgeBase.getId());
            starRecord.setKbUuid(kbUuid);
            knowledgeBaseStarRecordService.save(starRecord);

            star = true;
        } else {
            //Deleted means unstar
            knowledgeBaseStarRecordService.lambdaUpdate()
                    .eq(KnowledgeBaseStarRecord::getId, oldRecord.getId())
                    .set(KnowledgeBaseStarRecord::getIsDeleted, !oldRecord.getIsDeleted())
                    .update();
            star = oldRecord.getIsDeleted();
        }
        int starCount = star ? knowledgeBase.getStarCount() + 1 : knowledgeBase.getStarCount() - 1;
        ChainWrappers.lambdaUpdateChain(baseMapper)
                .eq(KnowledgeBase::getId, knowledgeBase.getId())
                .set(KnowledgeBase::getStarCount, starCount)
                .update();
        return star;
    }

    /**
     * 知识库问答限额判断
     */
    private void checkRequestTimesOrThrow() {
        String key = MessageFormat.format(RedisKeyConstant.AQ_ASK_TIMES, ThreadContext.getCurrentUserId(), LocalDateTimeUtil.format(LocalDateTime.now(), "yyyyMMdd"));
        String askTimes = stringRedisTemplate.opsForValue().get(key);
        String askQuota = SysConfigService.getByKey(QUOTA_BY_QA_ASK_DAILY);
        if (null != askTimes && null != askQuota && Integer.parseInt(askTimes) >= Integer.parseInt(askQuota)) {
            throw new BaseException(A_QA_ASK_LIMIT);
        }
        stringRedisTemplate.opsForValue().increment(key);
        stringRedisTemplate.expire(key, Duration.ofDays(1));
    }

    /**
     * 文档召回并将请求发送给LLM
     *
     * @param user         当前提问的用户
     * @param sseEmitter   sse emitter
     * @param qaRecordUuid 知识库uuid
     */
    @Async
    public void retrieveAndPushToLLM(User user, SseEmitter sseEmitter, String qaRecordUuid) {
        log.info("retrieveAndPushToLLM,qaRecordUuid:{},userId:{}", qaRecordUuid, user.getId());
        KnowledgeBaseQaRecord qaRecord = knowledgeBaseQaRecordService.getOrThrow(qaRecordUuid);
        KnowledgeBase knowledgeBase = getOrThrow(qaRecord.getKbUuid());
        AiModel aiModel = aiModelService.getByIdOrThrow(qaRecord.getAiModelId());

        Map<String, String> metadataCond = Map.of(AdiConstant.EmbeddingMetadataKey.KB_UUID, qaRecord.getKbUuid());
        int maxInputTokens = aiModel.getMaxInputTokens();
        int maxResults = knowledgeBase.getRagMaxResults();
        //maxResults < 1 表示由系统根据设置的模型maxInputTokens自动计算大小
        if (maxResults < 1) {
            maxResults = RAGService.getRetrieveMaxResults(qaRecord.getQuestion(), maxInputTokens);
        }

        SseAskParams sseAskParams = new SseAskParams();
        sseAskParams.setUuid(qaRecord.getUuid());
        sseAskParams.setAssistantChatParams(
                AssistantChatParams.builder()
                        .messageId(qaRecord.getKbUuid() + "_" + user.getUuid())
                        .systemMessage(StringUtils.EMPTY)
                        .userMessage(qaRecord.getQuestion())
                        .build()
        );
        sseAskParams.setLlmBuilderProperties(
                LLMBuilderProperties.builder()
                        .temperature(knowledgeBase.getLlmTemperature())
                        .build()
        );
        sseAskParams.setSseEmitter(sseEmitter);
        sseAskParams.setModelName(aiModel.getName());

        //用户问题过长，无需再召回文档，严格模式下直接返回异常提示,非严格模式请求LLM
        if (maxResults == 0) {
            log.info("用户问题过长，无需再召回文档，严格模式下直接返回异常提示,非严格模式请求LLM");
            if (knowledgeBase.getIsStrict()) {
                sseEmitterHelper.sendErrorAndComplete(user.getId(), sseEmitter, "提问内容过长，最多不超过 " + maxInputTokens + " tokens");
            } else {
                sseEmitterHelper.commonProcess(user, sseAskParams, (response, questionMeta, answerMeta) -> {
                    updateQaRecord(user, qaRecord, sseAskParams, null, response);
                });
            }
        } else {
            log.info("进行RAG请求,maxResults:{}", maxResults);
            AdiEmbeddingStoreContentRetriever contentRetriever = ragService.createRetriever(metadataCond, maxResults, knowledgeBase.getRagMinScore(), knowledgeBase.getIsStrict());
            sseEmitterHelper.ragProcess(contentRetriever, user, sseAskParams, (response, promptMeta, answerMeta) -> {
                updateQaRecord(user, qaRecord, sseAskParams, contentRetriever, response);
            });
        }
    }

    private void updateQaRecord(User user, KnowledgeBaseQaRecord qaRecord, SseAskParams sseAskParams, AdiEmbeddingStoreContentRetriever contentRetriever, String response) {

        List<String> tokenCountList = stringRedisTemplate.opsForList().range(MessageFormat.format(TOKEN_USAGE_KEY, sseAskParams.getUuid()), 0, -1);
        int inputTokenCount = 0;
        int outputTokenCount = 0;
        if (!CollectionUtils.isEmpty(tokenCountList) && tokenCountList.size() > 1) {
            inputTokenCount = Integer.parseInt(tokenCountList.get(tokenCountList.size() - 2));
            outputTokenCount = Integer.parseInt(tokenCountList.get(tokenCountList.size() - 1));
        }

        KnowledgeBaseQaRecord updateRecord = new KnowledgeBaseQaRecord();
        updateRecord.setId(qaRecord.getId());
        updateRecord.setPrompt(sseAskParams.getAssistantChatParams().getUserMessage());
        updateRecord.setPromptTokens(inputTokenCount);
        updateRecord.setAnswer(response);
        updateRecord.setAnswerTokens(outputTokenCount);
        knowledgeBaseQaRecordService.updateById(updateRecord);
        if (null != contentRetriever) {
            knowledgeBaseQaRecordService.createReferences(user, qaRecord.getId(), contentRetriever.getRetrievedEmbeddingToScore());
        }

        //用户本次请求消耗的token数指的是整个RAG过程中消耗的token数量，其中可能涉及到多次LLM请求
        int allToken = tokenCountList.stream().map(Integer::parseInt).reduce(0, Integer::sum);
        log.info("用户{}本次请示消耗总token:{}", user.getName(), allToken);
        userDayCostService.appendCostToUser(user, allToken);
    }

    public KnowledgeBase getOrThrow(String kbUuid) {
        return ChainWrappers.lambdaQueryChain(baseMapper)
                .eq(KnowledgeBase::getUuid, kbUuid)
                .eq(KnowledgeBase::getIsDeleted, false)
                .oneOpt().orElseThrow(() -> new BaseException(A_DATA_NOT_FOUND));
    }

    /**
     * Set update knowledge base stat signal
     *
     * @param kbUuid
     */
    public void updateStatistic(String kbUuid) {
        stringRedisTemplate.opsForSet().add(KB_STATISTIC_RECALCULATE_SIGNAL, kbUuid);
    }

    public int countTodayCreated() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime beginTime = LocalDateTime.of(now.getYear(), now.getMonth(), now.getDayOfMonth(), 0, 0, 0);
        LocalDateTime endTime = beginTime.plusDays(1);
        return baseMapper.countCreatedByTimePeriod(beginTime, endTime);
    }

    public int countAllCreated() {
        return baseMapper.countAllCreated();
    }

    /**
     * Update knowledge base stat
     */
    @Scheduled(fixedDelay = 60 * 1000)
    public void asyncUpdateStatistic() {
        Set<String> kbUuidList = stringRedisTemplate.opsForSet().members(KB_STATISTIC_RECALCULATE_SIGNAL);
        for (String kbUuid : kbUuidList) {
            baseMapper.updateStatByUuid(kbUuid);
            stringRedisTemplate.opsForSet().remove(KB_STATISTIC_RECALCULATE_SIGNAL, kbUuid);
        }
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
        LambdaQueryWrapper<KnowledgeBase> wrapper = new LambdaQueryWrapper<>();
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
