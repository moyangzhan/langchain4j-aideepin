<script setup lang="ts">
import { ref, watch } from 'vue'
import { NButton, NModal } from 'naive-ui'
import { useI18n } from 'vue-i18n'
import { useAuthStore } from '@/store'
import api from '@/api'

const props = defineProps<{
  show: boolean
  fileUrl: string
  fileName: string
}>()

const emit = defineEmits<{
  (e: 'update:show', value: boolean): void
}>()

const { t } = useI18n()
const authStore = useAuthStore()

const fullUrl = ref('')
const mimeType = ref('')
const textContent = ref('')

watch(() => [props.fileUrl, props.fileName], () => {
  textContent.value = ''
  mimeType.value = ''
  fullUrl.value = ''
  if (!props.fileUrl)
    return
  fullUrl.value = `${props.fileUrl}?token=${authStore.token}`
  const ext = props.fileName.substring(props.fileName.lastIndexOf('.') + 1)
  switch (ext) {
    case 'pdf':
      mimeType.value = 'application/pdf'
      break
    case 'doc':
    case 'docx':
      mimeType.value = 'application/msword'
      break
    case 'ppt':
    case 'pptx':
      mimeType.value = 'application/vnd.ms-powerpoint'
      break
    case 'xls':
    case 'xlsx':
      mimeType.value = 'application/vnd.ms-excel'
      break
    case 'html':
      mimeType.value = 'text/html'
      break
    case 'txt':
      mimeType.value = 'text/plain'
      api.loadFileContent(fullUrl.value).then((resp: any) => {
        textContent.value = resp.data
      }).catch(() => {
      })
      break
    default:
      mimeType.value = 'text/plain'
  }
}, { immediate: true })

function download() {
  const x = new window.XMLHttpRequest()
  x.open('GET', fullUrl.value, true)
  x.responseType = 'blob'
  x.onload = () => {
    const url = window.URL.createObjectURL(x.response)
    const a = document.createElement('a')
    a.href = url
    a.download = props.fileName
    a.click()
  }
  x.send()
}
</script>

<script lang="ts">
export default { name: 'FilePreviewModal' }
</script>

<template>
  <NModal
    :show="props.show" style="width: 90%;" preset="card"
    :title="`${t('workflow.filePreviewTitle')}${props.fileName}`"
    @update:show="(v: boolean) => emit('update:show', v)"
  >
    <div style="text-align: center;max-height:700px;overflow-y: auto">
      <div v-if="props.fileUrl && mimeType === 'text/plain'">
        {{ textContent }}
      </div>
      <object
        v-if="props.fileUrl && mimeType !== 'text/plain' && mimeType !== 'application/pdf'"
        :data="fullUrl" width="100%" height="90%" :type="mimeType"
      >
        <p>{{ t('workflow.browserNotSupportEmbed') }}</p>
      </object>
    </div>
    <template #footer>
      <NButton type="primary" text tag="a" size="small" @click="download">
        {{ t('workflow.clickToDownload') }}{{ props.fileName }}
      </NButton>
    </template>
  </NModal>
</template>
