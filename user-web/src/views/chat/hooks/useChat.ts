import { useChatStore } from '@/store'

export function useChat() {
  const chatStore = useChatStore()

  const getMsgsByConvAndIndex = (uuid: string, index: number) => {
    return chatStore.getMsgsByConvAndIndex(uuid, index)
  }

  const addMessage = (uuid: string, chat: Chat.ChatMessage, tail: boolean) => {
    chatStore.addMessage(uuid, chat, tail)
  }

  const updateMessage = (uuid: string, index: number, chat: Chat.ChatMessage) => {
    chatStore.updateMessage(uuid, index, chat)
  }

  const appendAnswer = (convUuid: string, userMessageUuid: string, childMessage: Chat.ChatMessage) => {
    chatStore.appendChild(convUuid, userMessageUuid, childMessage)
  }

  const unshiftAnswer = (convUuid: string, questionUuid: string, childMessage: Chat.ChatMessage) => {
    chatStore.unshiftChild(convUuid, questionUuid, childMessage)
  }

  const appendChunk = (convUuid: string, answerUuid: string, chunk: string, thinking = false) => {
    chatStore.appendChunk(convUuid, answerUuid, chunk, thinking)
  }

  const updateMessageSomeFields = (convUuid: string, messageUuid: string, chat: Partial<Chat.ChatMessage>) => {
    chatStore.updateMessageSomeFields(convUuid, messageUuid, chat)
  }

  return {
    addMessage,
    updateMessage,
    updateMessageSomeFields,
    appendChunk,
    getMsgsByConvAndIndex,
    appendAnswer,
    unshiftAnswer,
  }
}
