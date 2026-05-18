import { useChatStore } from '@/store'

export function useChat() {
  const chatStore = useChatStore()

  const getMsgsByCharacterAndIndex = (uuid: string, index: number) => {
    return chatStore.getMsgsByCharacterAndIndex(uuid, index)
  }

  const addMessage = (uuid: string, chat: Chat.ChatMessage, tail: boolean) => {
    chatStore.addMessage(uuid, chat, tail)
  }

  const updateMessage = (uuid: string, index: number, chat: Chat.ChatMessage) => {
    chatStore.updateMessage(uuid, index, chat)
  }

  const appendAnswer = (characterUuid: string, userMessageUuid: string, childMessage: Chat.ChatMessage) => {
    chatStore.appendChild(characterUuid, userMessageUuid, childMessage)
  }

  const unshiftAnswer = (characterUuid: string, questionUuid: string, childMessage: Chat.ChatMessage) => {
    chatStore.unshiftChild(characterUuid, questionUuid, childMessage)
  }

  const appendChunk = (characterUuid: string, answerUuid: string, chunk: string, thinking = false) => {
    chatStore.appendChunk(characterUuid, answerUuid, chunk, thinking)
  }

  const updateMessageSomeFields = (characterUuid: string, messageUuid: string, chat: Partial<Chat.ChatMessage>) => {
    chatStore.updateMessageSomeFields(characterUuid, messageUuid, chat)
  }

  return {
    addMessage,
    updateMessage,
    updateMessageSomeFields,
    appendChunk,
    getMsgsByCharacterAndIndex,
    appendAnswer,
    unshiftAnswer,
  }
}
