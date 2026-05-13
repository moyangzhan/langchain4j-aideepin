export interface Conversation {
  id: string
  uuid: string
  title: string
  aiSystemMessage: string
  tokens: number
  understandContextEnable: boolean
  createTime: string
  updateTime: string
}

export interface ConversationPreset {
  id: string
  uuid: string
  title: string
  remark: string
  createTime: string
  updateTime: string
}