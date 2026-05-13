declare namespace AiSearch {
  interface SearchEngineRespItem {
    title: string
    link: string
    snippet: string
    content: string
  }
  interface SearchEngineResp {
    items: SearchEngineRespItem[]
  }
  interface AiSearchRecord {
    uuid: string
    question: string
    searchEngineResp: SearchEngineResp
    answer: string
    createTime: string
    loading: boolean
    error: boolean
    aiModelPlatform: string
  }
  interface AiSearchResp {
    minId: number
    records: AiSearchRecord[]
  }
  interface AiSearchState {
    nextLoadingMaxId: number
    loadingRecords: boolean
    loadedAll: boolean
    sseRequesting: boolean
    records: AiSearchRecord[]
  }
}
