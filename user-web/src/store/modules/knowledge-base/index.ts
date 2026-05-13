import { defineStore } from 'pinia'

export const useKbStore = defineStore('kb-store', {
  state: (): KnowledgeBase.KbState => {
    return {
      selectedKbType: 'mine',
      activeKbUuid: 'default',
      myKbInfos: [],
      publicKbInfos: [],
      kbUuidToQaRecords: new Map<string, KnowledgeBase.QaRecordInfo[]>(),
      kbUuidToStarInfo: new Map<string, KnowledgeBase.KbStarInfo>(),
      qaRecordToEmbeddingRef: new Map<string, KnowledgeBase.QaRecordEmbeddingRef[]>(),
      qaRecordToGraphRef: new Map<string, KnowledgeBase.QaRecordGraphRef>(),
      loadingGraphRef: new Map<string, boolean>(),
      loadingRecords: new Map<string, boolean>(),
      loaddingKbList: false,
      reloadKbInfosSignal: false,
    }
  },

  getters: {
    getRecords(state: KnowledgeBase.KbState) {
      return (kbUuid: string) => {
        const records = state.kbUuidToQaRecords.get(kbUuid)
        if (records)
          return records

        return []
      }
    },
    getReferences(state: KnowledgeBase.KbState) {
      return (qaRecordUuid: string) => {
        const references = state.qaRecordToEmbeddingRef.get(qaRecordUuid)
        if (references)
          return references
        return []
      }
    },
    getGraphRef(state: KnowledgeBase.KbState) {
      return (qaRecordUuid: string) => {
        const graphRef = state.qaRecordToGraphRef.get(qaRecordUuid)
        if (graphRef)
          return graphRef
        return null
      }
    },
    isLoadingGraphRef(state: KnowledgeBase.KbState) {
      return (qaRecordUuid: string) => {
        const loading = state.loadingGraphRef.get(qaRecordUuid)
        if (loading)
          return loading
        return false
      }
    },
    getSelectedKb(state: KnowledgeBase.KbState) {
      let kbInfo = state.myKbInfos.find(item => item.uuid === state.activeKbUuid)
      if (kbInfo)
        return kbInfo
      kbInfo = state.publicKbInfos.find(item => item.uuid === state.activeKbUuid)
      if (kbInfo)
        return kbInfo
      return null
    },
  },

  actions: {
    setActive(kbUuid: string) {
      this.activeKbUuid = kbUuid
    },
    setLoadingKbList(status: boolean) {
      this.loaddingKbList = status
    },
    setLoadingRecords(currKbUuid: string, status: boolean) {
      this.loadingRecords.set(currKbUuid, status)
    },
    setReloadKbInfosSignal(signal: boolean) {
      this.reloadKbInfosSignal = signal
    },
    setMyKbInfos(infos: KnowledgeBase.Info[]) {
      this.myKbInfos = infos
    },
    setPublicKbInfos(infos: KnowledgeBase.Info[]) {
      this.publicKbInfos = infos
    },
    appendMyNewKbInfo(kbInfo: KnowledgeBase.Info) {
      this.myKbInfos.unshift(kbInfo)
    },
    appendRecord(kbUuid: string, record: KnowledgeBase.QaRecordInfo) {
      let existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords)
        existRecords = []
      existRecords.forEach(item => item.loading = false)
      existRecords.push(record)
    },
    appendRecords(kbUuid: string, records: KnowledgeBase.QaRecordInfo[]) {
      let existRecords = this.kbUuidToQaRecords.get(kbUuid)
      console.log('append records', records)
      if (!existRecords) {
        existRecords = []
        this.kbUuidToQaRecords.set(kbUuid, existRecords)
      }
      existRecords.push(...records.reverse())
    },
    appendChunk(kbUuid: string, tmpRecordUuid: string, chunk: string) {
      const existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords)
        return
      const hitRecord = existRecords.find((item: { uuid: string }) => item.uuid === tmpRecordUuid)
      if (hitRecord)
        hitRecord.answer = hitRecord.answer + chunk
    },
    updateRecord(kbUuid: string, tmpRecordUuid: string, source: KnowledgeBase.QaRecordInfo) {
      const existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords)
        return
      const hitRecord = existRecords.find((item: { uuid: string }) => item.uuid === tmpRecordUuid)
      if (hitRecord)
        Object.assign(hitRecord, source)
    },
    updateFirst(kbUuid: string, source: KnowledgeBase.QaRecordInfo) {
      const existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords || existRecords.length < 1)
        return
      Object.assign(existRecords[0], source)
    },

    unshiftRecord(kbUuid: string, record: KnowledgeBase.QaRecordInfo) {
      let existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords) {
        existRecords = []
        this.kbUuidToQaRecords.set(kbUuid, existRecords)
      }
      existRecords.unshift(record)
    },

    deleteRecord(kbUuid: string, recordUuid: string) {
      const existRecords = this.kbUuidToQaRecords.get(kbUuid)
      if (!existRecords)
        return

      const index = existRecords.findIndex((item: { uuid: string }) => item.uuid === recordUuid)
      existRecords.splice(index, 1)
    },

    clearRecords(kbUuid: string) {
      this.kbUuidToQaRecords.set(kbUuid, [])
    },

    appStarInfos(starInfos: KnowledgeBase.KbStarInfo[]) {
      starInfos.forEach((item) => {
        // Default true(from remote server)
        if (!item.star)
          item.star = true

        this.kbUuidToStarInfo.set(item.kbUuid, item)
      })
    },

    insertOrUpdateStarInfo(starInfo: KnowledgeBase.KbStarInfo) {
      const existData = this.kbUuidToStarInfo.get(starInfo.kbUuid)
      if (!existData)
        this.kbUuidToStarInfo.set(starInfo.kbUuid, starInfo)
      else
        existData.star = starInfo.star
    },

    setQaRecordReferences(qaRecordUuid: string, references: KnowledgeBase.QaRecordEmbeddingRef[]) {
      this.qaRecordToEmbeddingRef.set(qaRecordUuid, !references ? [] : references)
    },

    setQaRecordGraphRef(qaRecordUuid: string, graphRef: KnowledgeBase.QaRecordGraphRef) {
      this.qaRecordToGraphRef.set(qaRecordUuid, graphRef)
    },

    setLoadingGraphRef(qaRecordUuid: string, loading: boolean) {
      this.loadingGraphRef.set(qaRecordUuid, loading)
    },
  },
})
