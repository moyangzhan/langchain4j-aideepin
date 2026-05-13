<script lang="ts" setup>
import { ref, watch } from 'vue'
import { NModal } from 'naive-ui'
import EditConvDetail from './EditConvDetail.vue'
import { useAuthStore, useMcpStore } from '@/store'
import { emptyConv } from '@/utils/functions'
import api from '@/api'

interface Props {
  showModal: boolean
  conversation: Chat.Conversation
}
interface Emit {
  (ev: 'showModal', show: boolean): void
}
const props = withDefaults(defineProps<Props>(), {
  showModal: false,
})
const emit = defineEmits<Emit>()
const mcpStore = useMcpStore()
const authStore = useAuthStore()
const innerShow = ref<boolean>(props.showModal)
const tmpConv = ref<Chat.Conversation>(emptyConv())

function initEditConv(item: Chat.Conversation) {
  Object.assign(tmpConv.value, item)
}

function handleSubmitted() {
  emit('showModal', false)
}

watch(() => props.showModal, (val) => {
  innerShow.value = val
})

watch(() => props.conversation, (val) => {
  if (val)
    initEditConv(val)
})

watch(() => innerShow.value, (val) => {
  if (!val)
    emit('showModal', false)
})

// Load user MCP list when user is logged in
async function loadMyUserMcpList() {
  if (mcpStore.userMcpLoading || !authStore.token || mcpStore.myUserMcpList.length > 0)
    return
  try {
    mcpStore.setUserMcpLoading(true)
    const { data } = await api.userMcpList<Mcp.UserMcpListResp>(1, 200)
    if (data.records.length > 0)
      mcpStore.appendMyUserMcpList(data.records)
  } catch (error) {
    console.error(error)
  } finally {
    mcpStore.setUserMcpLoading(false)
  }
}
watch(
  () => authStore.token,
  () => {
    if (authStore.token)
      loadMyUserMcpList()
  },
  { immediate: true },
)
</script>

<template>
  <NModal v-model:show="innerShow" style="width: 90%; max-width: 640px" preset="card" :title="`${tmpConv.title} - 编辑`">
    <EditConvDetail :conversation="tmpConv" @submitted="handleSubmitted" />
  </NModal>
</template>
