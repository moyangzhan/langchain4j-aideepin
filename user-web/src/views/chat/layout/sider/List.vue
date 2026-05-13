<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NScrollbar } from 'naive-ui'
import { useRoute } from 'vue-router'
import { SvgIcon } from '@/components/common'
import { useAppStore, useAuthStore, useChatStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import EditConv from '@/views/chat/components/Header/EditConv.vue'
import api from '@/api'

const { isMobile } = useBasicLayout()
const route = useRoute()
const appStore = useAppStore()
const chatStore = useChatStore()
const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const mouseEnterKbUuid = ref<string>('')
const showEditModal = ref<boolean>(false)
const editConv = ref<Chat.Conversation>({} as Chat.Conversation)

async function handleSelect({ uuid }: Chat.Conversation) {
  console.log('click chat', uuid)
  if (isActive(uuid))
    return

  if (chatStore.active)
    chatStore.updateConv(chatStore.active, {})
  await chatStore.setActive(uuid)

  await checkAndLoadFirstPageMsgsByConv(uuid)

  if (isMobile.value)
    appStore.setSiderCollapsed(true)
}

function handleMouseEnter({ uuid }: Chat.Conversation) {
  mouseEnterKbUuid.value = uuid
}
function handleMouseLeave() {
  mouseEnterKbUuid.value = ''
}
function openEditView(item: Chat.Conversation) {
  if (!authStore.checkLoginOrShow())
    return
  showEditModal.value = true
  editConv.value = item
}

function isActive(uuid: string) {
  return chatStore.active === uuid
}

async function fetchHistory() {
  const { data: convs } = await api.fetchConvs<Chat.Conversation[]>()
  if (convs.length > 0) {
    chatStore.clearDefault()
    chatStore.addConvs(convs)

    const active = route.params.uuid as string
    console.log('List.vue active', active)
    if (active === 'default') {
      await handleSelect(convs[0])
    } else {
      // F5刷新页面时
      await checkAndLoadFirstPageMsgsByConv(active)
    }
  }
}

/**
 * 如果会话{uuid}的消息不存在，向服务端请求第一页
 */
async function checkAndLoadFirstPageMsgsByConv(uuid: string) {
  if (chatStore.loadingMsgs.has(uuid))
    return

  chatStore.addLoadingMsg(uuid)
  try {
    const minMsgUuid = chatStore.getCurConv?.minMsgUuid || ''
    const cacheMessages = chatStore.getMsgsByConv(uuid)
    if (cacheMessages.length === 0) {
      const { data } = await api.fetchMessages<Chat.ConvMsgListResp>(uuid, minMsgUuid, 20)
      data.msgList.forEach((messageRecord) => {
        chatStore.addMessage(uuid, messageRecord, false)
      })
      chatStore.updateConv(uuid, { minMsgUuid: data.minMsgUuid, loadedFirstPageMsg: true })
    }
  } finally {
    chatStore.deleteLoadingMsg(uuid)
  }
}

const convList = computed(() => chatStore.conversations)

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
  <EditConv v-model:showModal="showEditModal" :conversation="editConv" @show-modal="(show) => showEditModal = show" />
  <NScrollbar class="px-4">
    <div class="flex flex-col gap-2 text-sm">
      <template v-if="!convList.length">
        <div class="flex flex-col items-center mt-4 text-center text-neutral-300">
          <SvgIcon icon="ri:inbox-line" class="mb-2 text-3xl" />
          <span>{{ $t('common.noData') }}</span>
        </div>
      </template>
      <template v-else>
        <div v-for="(item, index) of convList" :key="index">
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
