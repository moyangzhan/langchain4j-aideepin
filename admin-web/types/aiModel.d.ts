export interface AiModelData {
  id: string
  type: string
  name: string
  platform: string
  remark: string
  createTime: string
  updateTime: string
  isEnable: boolean
  isFree: boolean
  setting: string
  inputTypes: string
  responseFormatTypes: string
  isReasoner: boolean
  isThinkingClosable: boolean
  isSupportWebSearch: boolean
  healthStatus?: string
  healthReason?: string

  //For ui
  inputTypeList: string[]
  responseFormatTypeList: string[]
}
