import { ss } from '@/utils/storage'
import { emptyAiModel, emptySysConfigInfo } from '@/utils/functions'

const LOCAL_NAME = 'appSetting'

export type Theme = 'light' | 'dark' | 'auto'

export type Language = 'zh-CN' | 'en-US'

export interface AppState {
  siderCollapsed: boolean
  theme: Theme
  language: Language | ''

  selectedSearchEngine: string
  selectedLLM: AiModelInfo
  selectedImageModel: AiModelInfo
  searchEngines: SearchEngineInfo[]
  llms: AiModelInfo[]
  imageModels: AiModelInfo[]
  sysConfigInfo: SysConfigInfo
}

export function defaultSetting(): AppState {
  return {
    siderCollapsed: false,
    theme: 'light',
    language: '',
    selectedSearchEngine: '',
    selectedLLM: emptyAiModel(),
    selectedImageModel: emptyAiModel(),
    searchEngines: [],
    llms: [],
    imageModels: [],
    sysConfigInfo: emptySysConfigInfo(),
  }
}

export function getLocalSetting(): AppState {
  const localSetting: AppState | undefined = ss.get(LOCAL_NAME)
  return { ...defaultSetting(), ...localSetting }
}

export function setLocalSetting(setting: AppState): void {
  ss.set(LOCAL_NAME, setting)
}
