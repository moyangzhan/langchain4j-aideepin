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

/**
 * Uniform properties shape for image models (adi_ai_model.properties of type='image' rows).
 * Every image model declares all five keys (empty/false means the field is unsupported and the form field is hidden).
 * 图片模型 properties 统一结构:所有图片模型固定五键(空数组/false 表示不支持,对应表单项隐藏)
 */
interface ImageModelProperties {
  /** 尺寸档位 / Size options */
  sizes: { value: string, label: string }[]
  /** 单次生成张数上限(>1 时展示数量滑杆) / Per-run image count cap (slider shown when > 1) */
  max_images: number
  /** 图生图参考图数量上限 / Reference-image upload limit */
  max_ref_images: number
  /** 是否展示随机种子 / Whether the seed field is shown */
  is_support_seed: boolean
  /** 质量档位(空数组 = 不支持) / Quality options (empty = unsupported) */
  qualities: string[]
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