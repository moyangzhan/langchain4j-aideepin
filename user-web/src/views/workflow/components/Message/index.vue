<script setup lang='ts'>
import { computed, ref } from 'vue'
import { NDropdown, NImage, NImageGroup, NList, NListItem } from 'naive-ui'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import TextComponent from '@/views/chat/components/Message/Text.vue'
import NoPic from '@/assets/no_pic.png'
import { SvgIcon } from '@/components/common'
import { useIconRender } from '@/hooks/useIconRender'
import { useAuthStore } from '@/store'
import { getRealFileUrl } from '@/utils/functions'
import { formatDuration } from '@/utils/format'
import { t } from '@/locales'
interface Props {
  wfRuntime: Workflow.WorkflowRuntime
  ioObject: any
  workflow?: Workflow.WorkflowInfo
  inversion?: boolean
  showAvatar?: boolean
  errorMsg?: string
  loading?: boolean
  // Aggregated run metrics (input/output tokens, total duration in ms)
  inputTokens?: number
  outputTokens?: number
  duration?: number
}
const props = withDefaults(defineProps<Props>(), {
  showAvatar: true,
})
const emit = defineEmits<Emit>()

interface Emit {
  (ev: 'delete'): void
}
const { iconRender } = useIconRender()
const authStore = useAuthStore()
const messageRef = ref<HTMLElement>()
const imageUrls = ref<string[]>([])
const fileUrls = ref<string[]>([])
const options = computed(() => {
  const common = [
    {
      label: t('common.delete'),
      key: 'delete',
      icon: iconRender({ icon: 'ri:delete-bin-line' }),
    },
  ]
  return common
})

function handleSelect(key: 'copyText' | 'setPublic' | 'setPrivate' | 'delete') {
  switch (key) {
    case 'delete':
      emit('delete')
  }
}

const txt = computed(() => {
  let result = ''
  for (const key in props.ioObject) {
    const content = props.ioObject[key] as Workflow.NodeIOData
    if (content.type === 4)
      extractFileUrls(content)
    else
      result += content.value
  }
  return result
})

function extractFileUrls(nodeIOData: Workflow.NodeIOData) {
  if (Array.isArray(nodeIOData.value)) {
    nodeIOData.value.forEach((url) => {
      const lc = url.toLowerCase()
      if (isImageUrl(lc))
        imageUrls.value.push(url)
      else
        fileUrls.value.push(url)
    })
  } else {
    console.warn('nodeIOData.value is not an array', nodeIOData)
  }
}

function isImageUrl(url: string) {
  const imageExtensions = ['.jpg', '.jpeg', '.png', '.gif', '.bmp', '.svg']
  const extension = url.slice(url.lastIndexOf('.'))
  return imageExtensions.includes(extension.toLowerCase())
}

function openFileInNewTab(filelUrl: string) {
  const x = new window.XMLHttpRequest()
  x.open('GET', `${getRealFileUrl(filelUrl)}?token=${authStore.token}`, true)
  x.responseType = 'blob'
  x.onload = () => {
    const url = window.URL.createObjectURL(x.response)
    const a = document.createElement('a')
    a.href = url
    a.download = filelUrl.split('/').pop() || 'file'
    a.click()
  }
  x.send()
}
</script>

<template>
  <div ref="messageRef" class="flex w-full mb-6 overflow-hidden" :class="[{ 'flex-row-reverse': inversion }]">
    <div
      v-if="showAvatar"
      class="flex items-center justify-center flex-shrink-0 h-8 overflow-hidden rounded-full basis-8"
      :class="[inversion ? 'ml-2' : 'mr-2']"
    >
      <AvatarComponent :name="inversion ? 'user' : (workflow ? workflow.title : '')" />
    </div>
    <div class="overflow-hidden text-sm " :class="[inversion ? 'items-end' : 'items-start']">
      <p class="text-xs text-[#b4bbc4]" :class="[inversion ? 'text-right' : 'text-left']">
        {{ wfRuntime.createTime }}
        <span v-if="inputTokens != null && inputTokens > 0" class="ml-1">📥 {{ inputTokens }}</span>
        <span v-if="outputTokens != null && outputTokens > 0" class="ml-1">📤 {{ outputTokens }}</span>
        <span v-if="duration != null && duration > 0" class="ml-1">⏱ {{ formatDuration(duration) }}</span>
      </p>
      <div class="flex items-center w-full" :class="[{ 'flex-row-reverse': inversion }]">
        <!-- 1、渲染文字 -->
        <div v-if="errorMsg" class="flex items-start gap-1 mt-2">
          <TextComponent :inversion="inversion" :error="true" :text="errorMsg" :loading="false" :as-raw-text="true" />
        </div>
        <div v-else-if="Object.keys(ioObject).length === 0" class="flex items-start gap-1 mt-2">
          <TextComponent :inversion="inversion" :error="true" :text="t('common.noContent')" :loading="loading" :as-raw-text="true" />
        </div>
        <TextComponent
          v-else-if="txt" :inversion="inversion" :error="false" :text="txt" :loading="loading"
          :as-raw-text="inversion ? true : false"
        />
        <NImageGroup v-if="imageUrls.length > 0" class="my-2">
          <NImage
            v-for="imageUrl in imageUrls" :key="imageUrl" style="height:100px"
            :src="`${getRealFileUrl(imageUrl)}?token=${authStore.token}`" :fallback-src="NoPic" object-fit="cover"
          />
        </NImageGroup>
        <div v-if="fileUrls.length > 0" class="flex flex-col space-y-2 mt-2">
          <NList hoverable clickable>
            <NListItem v-for="fileUrl in fileUrls" :key="fileUrl">
              <div class="flex items-center" @click="openFileInNewTab(fileUrl)">
                <SvgIcon icon="carbon:attachment" class="mr-1 text-sm" />
                <span>{{ fileUrl.split('/').pop() }}</span>
              </div>
            </NListItem>
          </NList>
        </div>
        <!-- 消息框侧边下拉选择列表 -->
        <div v-show="inversion" class="flex flex-col">
          <NDropdown
            trigger="click" :placement="!inversion ? 'right' : 'left'" :options="options"
            @select="handleSelect"
          >
            <button class="transition text-neutral-300 hover:text-neutral-800 dark:hover:text-neutral-200">
              <SvgIcon icon="ri:more-2-fill" />
            </button>
          </NDropdown>
        </div>
      </div>
      <slot />
    </div>
  </div>
</template>
