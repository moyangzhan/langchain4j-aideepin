import { ss } from '@/utils/storage'
import { emptyCharacter } from '@/utils/functions'

const LOCAL_NAME = 'chatStorage'

const defaultCharacter = emptyCharacter()
defaultCharacter.uuid = 'default'
defaultCharacter.title = '默认对话'

export function defaultState(): Chat.ChatState {
  return {
    active: defaultCharacter.uuid,
    usingContext: true,
    presetCharacters: [],
    characters: [defaultCharacter],
    chats: [{ uuid: defaultCharacter.uuid, data: [] }],
    loadingMsgs: new Set<string>(),
    msgToMemoryRef: new Map<string, Chat.MemoryEmbedding[]>(),
    msgToEmbeddingRef: new Map<string, KnowledgeBase.QaRecordEmbeddingRef[]>(),
    msgToGraphRef: new Map<string, KnowledgeBase.QaRecordGraphRef>(),
    loadingGraphRef: new Map<string, boolean>(),
  }
}

export function getDefaultCharacter(): Chat.Character {
  return defaultCharacter
}

export function getLocalState(): Chat.ChatState {
  const localState = ss.get(LOCAL_NAME)
  return { ...defaultState(), ...localState }
}

export function setLocalState(state: Chat.ChatState) {
  ss.set(LOCAL_NAME, state)
}

export function findMessageFromCharacter(character: Chat.CharacterWithMessages, messageUuid: string): Chat.ChatMessage | null {
  const questions = character.data
  for (const question of questions) {
    if (question.uuid === messageUuid) {
      return question
    } else {
      const result = question.children.find(child => child.uuid === messageUuid)
      if (result)
        return result
    }
  }
  return null
}
