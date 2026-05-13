export interface PresetParam {
  name: string
  title: string
  value: any
  require_encrypt: boolean
  encrypted: boolean
}
export interface McpCustomizedParamDefinition {
  name: string
  title: string
  require_encrypt: boolean
}
export interface McpInfo {
  id: string
  uuid: string
  title: string
  transportType: string
  sseUrl: string
  sseTimeout: number
  stdioCommand: string
  stdioArg: string
  presetParams: PresetParam[]
  customizedParamDefinitions: McpCustomizedParamDefinition[]
  installType: string
  website: string
  remark: string
  isEnable: boolean
  createTime: string
  updateTime: string
}

export interface McpSearchReq {
  title: string
  transportType: string
  installType: string
  isEnable: boolean
}

export interface McpAddOrEditReq {
  uuid: string
  title: string
  transportType: string
  sseUrl: string
  sseTimeout: number
  stdioCommand: string
  stdioArg: string
  presetParams: PresetParam[]
  customizedParamDefinitions: McpCustomizedParamDefinition[]
  installType: string
  repoUrl: string
  remark: string
  isEnable: boolean
}