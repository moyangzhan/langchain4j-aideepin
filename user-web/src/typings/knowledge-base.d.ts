declare namespace KnowledgeBase {
  interface Info {
    id: string
    uuid: string
    title: string
    remark: string
    isPublic: boolean
    isStrict: boolean
    starCount: number
    ownerUuid: string
    ownerName: string
    loadingRecords?: boolean
    itemCount: number
    embeddingCount: number
    ingestMaxOverlap: number
    ingestSplitStrategy: string
    ingestMaxSegmentSize: number
    ingestCustomSeparator: string
    ingestModelId: string
    ingestTokenEstimator: string
    retrieveMaxResults: number
    retrieveMinScore: number
    queryLlmTemperature: number
    querySystemMessage: string
    
    ingestModelName: string
  }
  interface InfoListResp {
    total: number,
    records: Info[]
  }
  interface Item {
    id: string
    uuid: string
    kbId: string
    kbUuid: string
    title: string
    brief: string
    remark: string
    embeddingStatus: string
    graphicalStatus: string
    embeddingStatusChangeTime: string
    graphicalStatusChangeTime: string
    embeddingHitCount: number
    graphHitCount: number
    wordCount: number
    isEnabled: boolean
    enabledChangeTime: string
    segmentMode?: string
    childMaxChunkSize?: number
    autoGenerateQa?: boolean
    failReason?: string
    sourceFileName: string
    sourceFileUuid: string
    sourceFileUrl: string
    createTime: string
    updateTime: string
  }
  interface KbItemEditReq {
    id?: string
    kbId: string
    title: string
    remark?: string
    segmentMode?: string
    autoGenerateQa?: boolean
  }
  interface KbEmbedding {
    embeddingId: string
    embedding: number[]
    text: string
    hitCount: number
    wordCount: number
  }
  interface SegmentQuestion {
    id: string
    uuid: string
    answerSegmentId: string
    position: number
    content: string
    wordCount: number
    hitCount: number
    createTime: string
    updateTime: string
  }
  interface SegmentChildChunk {
    id: string
    uuid: string
    parentSegmentId: string
    position: number
    content: string
    wordCount: number
    hitCount: number
    createTime: string
    updateTime: string
  }
  interface Segment {
    id: string
    uuid: string
    docUuid: string
    position: number
    content: string
    wordCount: number
    hitCount: number
    isEnabled: boolean
    enabledChangeTime: string
    embeddingStatus: string
    graphicalStatus: string
    failReason?: string
    createTime: string
    updateTime: string
    questions?: SegmentQuestion[]
    children?: SegmentChildChunk[]
  }
  interface KbEdge {
    sourceName: string
    targetName: string
    description: string
    weight: number
  }
  interface KbVertex {
    name: string
    description: string
  }
  interface KbItemGraphResp {
    vertices: KbVertex[]
    edges: KbEdge[]
  }
  interface QaRecordListResp {
    total: number,
    records: KnowledgeBase.QaRecordInfo[]
  }
  interface QaRecordInfo {
    id: string
    uuid: string
    kbId: string
    kbUuid: string
    question: string
    answer: string
    createTime: string
    loading?: boolean
    error?: boolean
    aiModelPlatform?: string
    promptTokens?: number
    answerTokens?: number
    //SSE 实时写入的 token 数据 | Token data from SSE live stream
    inputTokens?: number
    outputTokens?: number
    //调用耗时（毫秒） | Call duration (ms)
    duration?: number
  }

  interface QaRecordEmbeddingRef {
    embeddingId: string
    text: string
    /** 命中的向量化单元：qa=命中问题，parent_child=命中子块；text 模式不填 */
    matchedText?: string
    /** 引用来源文档的分段模式：text | qa | parent_child */
    segmentMode?: string
  }

  interface QaRecordGraphRef {
    vertices: KbVertex[]
    edges: KbEdge[]
  }

  interface KbState {
    selectedKbType: string
    activeKbUuid: string
    myKbInfos: Info[]
    publicKbInfos: Info[]
    kbUuidToQaRecords: Map<string, QaRecordInfo[]>
    kbUuidToStarInfo: Map<string, KbStarInfo>
    qaRecordToEmbeddingRef: Map<string, KnowledgeBase.QaRecordEmbeddingRef[]>
    qaRecordToGraphRef: Map<string, KnowledgeBase.QaRecordGraphRef>
    loadingGraphRef: Map<string, boolean>
    loadingRecords: Map<string, boolean>
    loaddingKbList: boolean
    reloadKbInfosSignal: boolean
  }

  interface KbStarInfo {
    kbUuid: string
    kbTitle: string
    star: boolean
  }

  interface KbStarListResp {
    total: number
    records: KnowledgeBase.KbStarInfo[]
  }
}