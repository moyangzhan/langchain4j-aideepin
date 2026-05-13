import { t } from '@/locales'

export function getDefaultModelPlatforms() {
  return [
    {
      label: t('constants.openai'),
      value: 'openai',
    },
    {
      label: t('constants.dashscope'),
      value: 'dashscope',
    },
    {
      label: t('constants.qianfan'),
      value: 'qianfan',
    },
    {
      label: t('constants.ollama'),
      value: 'ollama',
    },
    {
      label: t('constants.deepseek'),
      value: 'deepseek',
    },
    {
      label: t('constants.siliconflow'),
      value: 'siliconflow',
    },
  ]
}

/** @deprecated Use getDefaultModelPlatforms() instead */
export const DEFAULT_MODEL_PLATFORMS = [
  {
    label: 'Openai',
    value: 'openai',
  },
  {
    label: 'DashScope',
    value: 'dashscope',
  },
  {
    label: 'Qianfan',
    value: 'qianfan',
  },
  {
    label: 'Ollama',
    value: 'ollama',
  },
  {
    label: 'DeepSeek',
    value: 'deepseek',
  },
  {
    label: 'SiliconFlow',
    value: 'siliconflow',
  },
]

export function getModelTypes() {
  return [
    {
      label: t('constants.text'),
      value: 'text',
    },
    {
      label: t('constants.image'),
      value: 'image',
    },
    {
      label: t('constants.embedding'),
      value: 'embedding',
    },
    {
      label: t('constants.rerank'),
      value: 'rerank',
    },
    {
      label: t('constants.tts'),
      value: 'tts',
    },
    {
      label: t('constants.asr'),
      value: 'asr',
    },
  ]
}

/** @deprecated Use getModelTypes() instead */
export const MODEL_TYPES = [
  {
    label: 'Text',
    value: 'text',
  },
  {
    label: 'Image',
    value: 'image',
  },
  {
    label: 'Embedding',
    value: 'embedding',
  },
  {
    label: 'Rerank',
    value: 'rerank',
  },
  {
    label: 'Text-to-Speech',
    value: 'tts',
  },
  {
    label: 'Speech Recognition',
    value: 'asr',
  },
]

export function getModelInputTypes() {
  return [
    {
      label: t('constants.text'),
      value: 'text',
    },
    {
      label: t('constants.image'),
      value: 'image',
    },
    {
      label: t('constants.audio'),
      value: 'audio',
    },
    {
      label: t('constants.video'),
      value: 'video',
    },
  ]
}

/** @deprecated Use getModelInputTypes() instead */
export const MODEL_INPUT_TYPES = [
  {
    label: 'Text',
    value: 'text',
  },
  {
    label: 'Image',
    value: 'image',
  },
  {
    label: 'Audio',
    value: 'audio',
  },
  {
    label: 'Video',
    value: 'video',
  },
]

export function getModelResponseFormatTypes() {
  return [
    {
      label: t('constants.text'),
      value: 'text',
    },
    {
      label: t('constants.json'),
      value: 'json_object',
    },
  ]
}

/** @deprecated Use getModelResponseFormatTypes() instead */
export const MODEL_RESPONSE_FORMAT_TYPES = [
  {
    label: 'Text',
    value: 'text',
  },
  {
    label: 'JSON',
    value: 'json_object',
  },
]

export function getMcpTransportType() {
  return [
    {
      label: t('constants.sse'),
      value: 'sse',
    },
    {
      label: t('constants.stdio'),
      value: 'stdio',
    },
  ]
}

/** @deprecated Use getMcpTransportType() instead */
export const mcpTransportType = [
  {
    label: 'SSE (Network)',
    value: 'sse',
  },
  {
    label: 'Standard I/O',
    value: 'stdio',
  },
]

export function getMcpInstallType() {
  return [
    {
      label: t('constants.docker'),
      value: 'docker',
    },
    {
      label: t('constants.local'),
      value: 'local',
    },
    {
      label: t('constants.remote'),
      value: 'remote',
    },
    {
      label: t('constants.wasm'),
      value: 'wasm',
    },
  ]
}

/** @deprecated Use getMcpInstallType() instead */
export const mcpInstallType = [
  {
    label: 'Docker',
    value: 'docker',
  },
  {
    label: 'Local',
    value: 'local',
  },
  {
    label: 'Remote',
    value: 'remote',
  },
  {
    label: 'WebAssembly',
    value: 'wasm',
  },
]

export function getSynthesizerSide() {
  return [
    {
      label: t('constants.client'),
      value: 'client',
    },
    {
      label: t('constants.server'),
      value: 'server',
    },
  ]
}

/** @deprecated Use getSynthesizerSide() instead */
export const synthesizerSide = [
  {
    label: 'Client',
    value: 'client',
  },
  {
    label: 'Server',
    value: 'server',
  },
]

export function getYesNo() {
  return [
    {
      label: t('constants.yes'),
      value: true,
    },
    {
      label: t('constants.no'),
      value: false,
    },
  ]
}

/** @deprecated Use getYesNo() instead */
export const YES_NO = [
  {
    label: 'Yes',
    value: true,
  },
  {
    label: 'No',
    value: false,
  },
]
