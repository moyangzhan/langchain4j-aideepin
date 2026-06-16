import { defineStore } from 'pinia'
import type { AppState, Language, Theme } from './helper'
import { getLocalSetting, setLocalSetting } from './helper'
import { AUDIO_SYNTHESIZER_SIDE } from '@/utils/constant'
import { store } from '@/store'

export function detectBrowserLocale(): Language | null {
  const browserLangs = navigator.languages || [navigator.language]
  for (const lang of browserLangs) {
    const lower = lang.toLowerCase()
    if (lower.startsWith('zh'))
      return 'zh-CN'
    if (lower.startsWith('en'))
      return 'en-US'
  }
  return null
}

export const useAppStore = defineStore('app-store', {

  state: (): AppState => getLocalSetting(),

  getters: {
    imageModelByPrefix(state: AppState) {
      return (prefix: string) => {
        return state.imageModels.find(item => item.modelName.indexOf(prefix) === 0)
      }
    },
    getLLM(state: AppState) {
      return (modelId: string) => {
        return state.llms.find(item => item.modelId === modelId)
      }
    },
    getLLMById(state: AppState) {
      return (id: string) => {
        return state.llms.find(item => item.modelId === id)
      }
    },
    getLLMByName(state: AppState) {
      return (name: string) => {
        return state.llms.find(item => item.modelName === name)
      }
    },
    getLLMByPlatformAndName(state: AppState) {
      return (platform: string, name: string) => {
        // 兼容只有 name 的情况
        return state.llms.find(item => (!platform || item.modelPlatform === platform) && item.modelName === name)
      }
    },
    getFirstLLM(state: AppState) {
      // Optional `type` arg restricts to a model type ('text' | 'vision' | …).
      // Workflow text nodes need this — without it the first vision model wins
      // when it happens to be free+healthy and ordered earlier in /model/llms.
      return (type?: string) => {
        const enableList = state.llms.filter(item =>
          item.enable
          && item.healthStatus !== 'UNHEALTHY'
          && (!type || item.type === type),
        )
        const freeLLM = enableList.find(item => item.isFree)
        if (freeLLM)
          return freeLLM
        else
          return enableList[0]
      }
    },
    getImageModelByName(state: AppState) {
      return (name: string) => {
        return state.imageModels.find(item => item.modelName === name)
      }
    },
    audioSynthesizerSide(state: AppState) {
      return state.sysConfigInfo.ttsSetting.synthesizer_side || AUDIO_SYNTHESIZER_SIDE.client
    },
    ttsSetting(state: AppState) {
      return state.sysConfigInfo.ttsSetting
    },
    availableVoices(state: AppState) {
      return state.sysConfigInfo.availableVoices || []
    },
  },

  actions: {
    setSysConfig(sysConfig: SysConfigInfo) {
      this.sysConfigInfo = sysConfig
      this.recordState()
    },
    setSiderCollapsed(collapsed: boolean) {
      this.siderCollapsed = collapsed
      this.recordState()
    },

    setTheme(theme: Theme) {
      this.theme = theme
      this.recordState()
    },

    setLanguage(language: Language) {
      if (this.language !== language) {
        this.language = language
        this.recordState()
      }
    },
    initLocale(defaultLocale: string) {
      if (!this.language) {
        this.language = (defaultLocale || 'zh-CN') as Language
        this.recordState()
      }
    },
    applyUserLocale(userLocale: string) {
      const effective = (userLocale || this.language || 'zh-CN') as Language
      if (this.language !== effective) {
        this.language = effective
        this.recordState()
      }
    },
    recordState() {
      setLocalSetting(this.$state)
    },
    setSelectedSearchEngine(selected: string) {
      this.selectedSearchEngine = selected
    },
    setSelectedLLM(selected: string) {
      const selectedModel = this.llms.find(item => item.modelId === selected)
      if (selectedModel)
        this.selectedLLM = selectedModel

      this.recordState()
    },
    setSelectedImageModel(selected: string) {
      const selectedModel = this.imageModels.find(item => item.modelName === selected)
      if (selectedModel)
        this.selectedImageModel = selectedModel
      this.recordState()
    },
    setSearchEngines(engines: SearchEngineInfo[]) {
      engines.forEach((item) => {
        item.disabled = !item.enable
        item.label = item.name
        item.key = item.name
      })
      this.searchEngines = engines
      if (!this.selectedSearchEngine) {
        const name = this.searchEngines.find(item => item.enable)?.name
        if (name)
          this.selectedSearchEngine = name
      }
      this.recordState()
    },
    setLLMs(llms: AiModelInfo[]) {
      llms.forEach((item) => {
        item.disabled = !item.enable || item.healthStatus === 'UNHEALTHY'
        item.label = item.modelTitle || item.modelName
        item.key = item.modelId
        item.value = item.modelId
      })
      this.llms = llms
      if (this.selectedLLM.modelId === 'default') {
        const selectedModel = this.llms.find(item => item.enable)
        if (selectedModel)
          this.selectedLLM = selectedModel
      }
      this.recordState()
    },
    setImageModels(imageModels: AiModelInfo[]) {
      imageModels.forEach((item) => {
        item.disabled = !item.enable
        item.label = item.modelTitle || item.modelName
        item.key = item.modelName
        item.value = item.modelName
      })
      this.imageModels = imageModels
      if (this.selectedImageModel.modelId === 'default') {
        const selectedModel = this.imageModels.find(item => item.enable)
        if (selectedModel)
          this.selectedImageModel = selectedModel
      }
      this.recordState()
    },
  },
})

export function useAppStoreWithOut() {
  return useAppStore(store)
}
