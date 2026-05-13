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
    sourceFileName: string
    sourceFileUuid: string
    sourceFileUrl: string
  }
  interface KbItemEditReq {
    id?: string
    kbId: string
    title: string
    remark?: string
  }
  interface KbEmbedding {
    embeddingId: string
    embedding: number[]
    text: string
  }
  interface KbEdge {
    id: number
    label: string
    startId: number
    endId: number
    description: string
  }
  interface KbVertex {
    id: number
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
  }

  interface QaRecordEmbeddingRef {
    embeddingId: string
    text: string
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