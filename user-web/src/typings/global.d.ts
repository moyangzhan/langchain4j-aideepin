interface Window {
  $loadingBar?: import('naive-ui').LoadingBarProviderInst;
  $dialog?: import('naive-ui').DialogProviderInst;
  $message?: import('naive-ui').MessageProviderInst;
  $notification?: import('naive-ui').NotificationProviderInst;
}
interface PageResponse {
  total: number
  size: number
  current: number
  pages: number
  records: []
}

interface ImageGenerationParams {
  interactingMethod: number
  modelName: string
  prompt: string
  size: string
  number: number
  seed?: number
  quality?: string
  dynamicParams?: any
}

interface CreateImageResult {
  uuid: string
}

interface AuthState {
  token: string
  showLoginModal: boolean
}

interface AiModelInfo {

  //from api
  modelId: string
  modelName: string
  modelTitle: string
  modelPlatform: string
  enable: boolean
  isFree: boolean
  /**
   * Model usage type: 'text' | 'image' | 'vision' | 'embedding' | 'rerank' | 'asr' | 'tts'.
   * Mirrors backend ai_model.type; use this (not inputTypes) to filter by purpose —
   * many text models support image input and would be wrongly excluded by inputTypes.
   */
  type: string
  inputTypes: string
  isReasoner: boolean
  isThinkingClosable: boolean
  isSupportWebSearch: boolean
  properties: object
  healthStatus?: string
  healthReason?: string

  //for NSelector
  value: string
  key: string
  label: string
  disabled: boolean
}

interface SearchEngineInfo {

  //from api
  name: string,
  enable: boolean,

  //for NSelector
  key: string,
  label: string,
  disabled: boolean
}

interface FileUploaded {
  url: string
  uuid: string
}

interface AudioPlayState {
  audioUrl: string
  audioUuid: string // UUID of the audio file
  playDuration: number
  playing: boolean
  audio: any // Audio object
  text: string // The text content corresponding to the audio
  showText: boolean // Whether to show the text

  msgPart: string // 聊天时不断接收到的消息片段，即时播放时使用
  audioFrame: string // 聊天时不断接收到的音频片段(已经过base64编码)，即时播放时使用
}

interface TtsSetting {
  synthesizer_side: string // TTS synthesizer, eg: client or server
  model_name: string
  platform: string // TTS model platform
}

interface AsrSetting {
  model_name: string
  platform: string
  max_record_duration: number // Maximum recording duration in seconds
  max_file_size: number // Maximum file size in bytes
}

interface ModelVoice {
  name: string
  remark: string
  param_name: string
  lang: string
}

interface SysConfigInfo {
  ttsSetting: TtsSetting
  asrSetting: AsrSetting
  availableVoices: ModelVoice[]
  defaultLocale: string
}