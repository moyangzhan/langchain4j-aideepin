<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NScrollbar } from 'naive-ui'
import { useRoute } from 'vue-router'
import { SvgIcon } from '@/components/common'
import { useAppStore, useAuthStore, useChatStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import EditConv from '@/views/chat/components/Header/EditConv.vue'
import api from '@/api'
import { t } from '@/locales'
const { isMobile } = useBasicLayout()
const route = useRoute()
const appStore = useAppStore()
const chatStore = useChatStore()
const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const mouseEnterKbUuid = ref<string>('')
const showEditModal = ref<boolean>(false)
const editCharacter = ref<Chat.Character>({} as Chat.Character)

async function handleSelect({ uuid }: Chat.Character) {
  console.log('click chat', uuid)
  if (isActive(uuid))
    return

  if (chatStore.active)
    chatStore.updateCharacter(chatStore.active, {})
  await chatStore.setActive(uuid)

  await checkAndLoadFirstPageMsgsByCharacter(uuid)

  if (isMobile.value)
    appStore.setSiderCollapsed(true)
}

function handleMouseEnter({ uuid }: Chat.Character) {
  mouseEnterKbUuid.value = uuid
}
function handleMouseLeave() {
  mouseEnterKbUuid.value = ''
}
function openEditView(item: Chat.Character) {
  if (!authStore.checkLoginOrShow())
    return
  showEditModal.value = true
  editCharacter.value = item
}

function isActive(uuid: string) {
  return chatStore.active === uuid
}

async function fetchHistory() {
  const { data: characters } = await api.fetchCharacters<Chat.Character[]>()
  if (characters.length > 0) {
    chatStore.clearDefault()
    chatStore.addCharacters(characters)

    const active = route.params.uuid as string
    console.log('List.vue active', active)
    if (active === 'default') {
      await handleSelect(characters[0])
    } else {
      // F5刷新页面时
      await checkAndLoadFirstPageMsgsByCharacter(active)
    }
  }
}

/**
 * 如果会话{uuid}的消息不存在，向服务端请求第一页
 */
async function checkAndLoadFirstPageMsgsByCharacter(uuid: string) {
  if (chatStore.loadingMsgs.has(uuid))
    return

  chatStore.addLoadingMsg(uuid)
  try {
    const minMsgUuid = chatStore.getCurCharacter?.minMsgUuid || ''
    const cacheMessages = chatStore.getMsgsByCharacter(uuid)
    if (cacheMessages.length === 0) {
      const { data } = await api.fetchMessages<Chat.CharacterMsgListResp>(uuid, minMsgUuid, 20)
      data.msgList.forEach((messageRecord) => {
        chatStore.addMessage(uuid, messageRecord, false)
      })
      chatStore.updateCharacter(uuid, { minMsgUuid: data.minMsgUuid, loadedFirstPageMsg: true })
    }
  } finally {
    chatStore.deleteLoadingMsg(uuid)
  }
}

const characterList = computed(() => chatStore.characters)

watch(
  () => authStoreRef.value.token,
  (newVal) => {
    if (newVal) {
      console.log('token change, reaload')
      fetchHistory()
    }
  },
)

onMounted(() => {
  console.log('chat list onMounted')
  if (authStoreRef.value.token)
    fetchHistory()
})
</script>

<template>
  <EditConv v-model:showModal="showEditModal" :character="editCharacter" @show-modal="(show) => showEditModal = show" />
  <NScrollbar class="px-4">
    <div class="flex flex-col gap-2 text-sm">
      <template v-if="!characterList.length">
        <div class="flex flex-col items-center mt-4 text-center text-neutral-300">
          <SvgIcon icon="ri:inbox-line" class="mb-2 text-3xl" />
          <span>{{ t('common.noData') }}</span>
        </div>
      </template>
      <template v-else>
        <div v-for="(item, index) of characterList" :key="index">
          <a
            class="relative flex items-center gap-3 px-3 py-3 break-all border rounded-md cursor-pointer hover:bg-neutral-100 group dark:border-neutral-800 dark:hover:bg-[#24272e]"
            :class="isActive(item.uuid) && ['border-[#4b9e5f]', 'bg-neutral-100', 'text-[#4b9e5f]', 'dark:bg-[#24272e]', 'dark:border-[#4b9e5f]']"
            @click="handleSelect(item)" @mouseenter="handleMouseEnter(item)" @mouseleave="handleMouseLeave"
          >
            <span>
              <SvgIcon icon="ri:message-3-line" />
            </span>
            <div class="relative flex-1 overflow-hidden break-all text-ellipsis whitespace-nowrap">
              <span>{{ item.title }}</span>
            </div>
            <div v-if="mouseEnterKbUuid === item.uuid || isMobile" class="absolute z-10 flex visible right-1 pd-2">
              <button class="p-1">
                <SvgIcon icon="carbon:edit" @click.stop="openEditView(item)" />
              </button>
            </div>
          </a>
        </div>
      </template>
    </div>
  </NScrollbar>
</template>
