import { defineStore } from 'pinia'

export const useAiSearchStore = defineStore('ai-search-store', {
  state: (): AiSearch.AiSearchState => {
    return {
      nextLoadingMaxId: 0,
      loadingRecords: false,
      loadedAll: false,
      sseRequesting: false,
      records: [],
    }
  },
  getters: {
  },

  actions: {
    setMaxId(maxId: number) {
      this.nextLoadingMaxId = maxId
    },
    setSseRequesting(requesting: boolean) {
      this.sseRequesting = requesting
    },
    setLoadingRecords(loading: boolean) {
      this.loadingRecords = loading
    },
    setLoadedAll() {
      this.loadedAll = true
    },
    appendRecord(record: AiSearch.AiSearchRecord) {
      this.records.push(record)
    },
    appendRecords(records: AiSearch.AiSearchRecord[]) {
      this.records.push(...records.reverse())
    },
    updateRecord(record: AiSearch.AiSearchRecord) {
      const hitRecord = this.records.find(item => item.uuid === record.uuid)
      if (hitRecord)
        Object.assign(hitRecord, record)
    },
    deleteRecord(recordUuid: string) {
      const index = this.records.findIndex(item => item.uuid === recordUuid)
      this.records.splice(index, 1)
    },
    appendChunk(recordUuid: string, chunk: string) {
      const hitRecord = this.records.find(item => item.uuid === recordUuid)
      if (hitRecord)
        hitRecord.answer = hitRecord.answer + chunk
    },
    setSourceSites(recordUuid: string, sourceSites: AiSearch.SearchEngineRespItem[]) {
      const hitRecord = this.records.find(item => item.uuid === recordUuid)
      if (hitRecord)
        hitRecord.searchEngineResp = { items: sourceSites }
    },
  },
})
