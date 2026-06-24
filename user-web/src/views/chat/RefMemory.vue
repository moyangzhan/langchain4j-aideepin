<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NCollapse, NCollapseItem } from 'naive-ui'
import { format, parseISO } from 'date-fns'
import { useChatStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { SvgIcon } from '@/components/common'

interface Props {
  msgUuid: string
}
const props = withDefaults(defineProps<Props>(), {
  msgUuid: '',
})

const chatStore = useChatStore()
const loading = ref<boolean>(false)
const memoryEmbeddings = ref<Chat.MemoryEmbedding[]>([])

// Split memory hits into two groups for display.
// 把命中的记忆按类型分组,语义记忆和情景记忆分别展示。
const semanticMemoryItems = computed(() => memoryEmbeddings.value.filter(m => m.memoryType !== 'episodic'))
const episodicMemoryItems = computed(() => memoryEmbeddings.value.filter(m => m.memoryType === 'episodic'))

// 把后端 LocalDateTime ISO 串（如 "2026-06-24T13:44:19.620981600"）转成
// 人类友好的 "yyyy-MM-dd HH:mm:ss"，与项目其他时间显示口径一致。
// 解析失败时（数据格式异常 / 缺失毫秒微秒等）原样返回，保证不空。
// <p>
// Format the backend's LocalDateTime ISO string into "yyyy-MM-dd HH:mm:ss",
// matching the rest of the app. Falls back to the raw string if parsing fails.
function formatCreatedAt(raw?: string | null): string {
  if (!raw)
    return ''
  try {
    const date = parseISO(raw)
    if (Number.isNaN(date.getTime()))
      return raw
    return format(date, 'yyyy-MM-dd HH:mm:ss')
  } catch {
    return raw
  }
}

async function load(msgUuid: string) {
  if (!msgUuid) {
    memoryEmbeddings.value = []
    return
  }
  // 优先取缓存，未命中再请求接口
  // Prefer cached refs; fall back to the API only if the cache is empty.
  memoryEmbeddings.value = chatStore.getMemory(msgUuid)
  if (memoryEmbeddings.value.length > 0)
    return

  loading.value = true
  try {
    const { data } = await api.memoryEmbeddingRef(msgUuid)
    chatStore.setMemoryRefs(msgUuid, data)
    // 加载完成后，若用户期间没切到别的消息，再刷新本地引用
    // Only refresh local state if the user hasn't switched to a different message in the meantime.
    if (msgUuid === props.msgUuid)
      memoryEmbeddings.value = chatStore.getMemory(msgUuid)
  } finally {
    loading.value = false
  }
}

onMounted(() => load(props.msgUuid))
watch(() => props.msgUuid, newUuid => load(newUuid))
</script>

<template>
  <div v-show="memoryEmbeddings.length === 0" class="flex items-center justify-center h-64">
    <span v-show="!loading">{{ t('common.noData') }}</span>
    <SvgIcon v-show="loading" icon="line-md:loading-loop" class="text-2xl text-green-800 w-12 h-12" />
  </div>
  <div v-show="memoryEmbeddings.length > 0">
    <!-- Semantic memory group -->
    <div v-if="semanticMemoryItems.length > 0" class="mb-4">
      <div class="font-semibold text-sm mb-2 text-gray-700 dark:text-gray-300">
        {{ t('chat.semanticMemory') }} ({{ semanticMemoryItems.length }})
      </div>
      <NCollapse :default-expanded-names="['sem_0']">
        <NCollapseItem
          v-for="(reference, idx) of semanticMemoryItems" :key="reference.embeddingId"
          :title="`${t('chat.memory')} ${idx + 1}`"
          :name="`sem_${idx}`"
        >
          {{ reference.text }}
        </NCollapseItem>
      </NCollapse>
    </div>
    <!-- Episodic memory group -->
    <div v-if="episodicMemoryItems.length > 0">
      <div class="font-semibold text-sm mb-2 text-gray-700 dark:text-gray-300">
        {{ t('chat.episodicMemory') }} ({{ episodicMemoryItems.length }})
      </div>
      <NCollapse :default-expanded-names="['epi_0']">
        <NCollapseItem
          v-for="(reference, idx) of episodicMemoryItems" :key="reference.embeddingId"
          :name="`epi_${idx}`"
        >
          <template #header>
            <div class="flex items-center gap-2 text-sm">
              <span>{{ formatCreatedAt(reference.createdAt) || `${t('chat.episodicMemory')} ${idx + 1}` }}</span>
            </div>
          </template>
          {{ reference.text }}
        </NCollapseItem>
      </NCollapse>
    </div>
  </div>
</template>
