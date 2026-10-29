<script setup lang="ts">
import { computed, ref, watch } from 'vue'
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

const CSV_PREVIEW_MAX_LINES = 50

const { t } = useI18n()
const authStore = useAuthStore()

const fullUrl = ref('')
const previewKind = ref<'text' | 'embed' | 'unsupported'>('unsupported')
const textContent = ref('')
const truncated = ref(false)

watch(() => [props.fileUrl, props.fileName], () => {
  textContent.value = ''
  truncated.value = false
  fullUrl.value = ''
  if (!props.fileUrl)
    return
  fullUrl.value = `${props.fileUrl}?token=${authStore.token}`
  const ext = props.fileName.substring(props.fileName.lastIndexOf('.') + 1)
  if (ext === 'txt' || ext === 'csv') {
    previewKind.value = 'text'
    api.loadFileContent(fullUrl.value).then((resp: any) => {
      const text = String(resp.data ?? '')
      if (ext !== 'csv') {
        textContent.value = text
        return
      }
      const lines = text.split('\n')
      truncated.value = lines.length > CSV_PREVIEW_MAX_LINES
      textContent.value = lines.slice(0, CSV_PREVIEW_MAX_LINES).join('\n')
    }).catch(() => {
    })
    return
  }
  if (ext === 'pdf' || ext === 'html') {
    previewKind.value = 'embed'
    return
  }
  previewKind.value = 'unsupported'
}, { immediate: true })

const embedMime = computed(() => (props.fileName.endsWith('.pdf') ? 'application/pdf' : 'text/html'))

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
      <pre v-if="previewKind === 'text'" style="text-align:left;white-space:pre;overflow-x:auto;margin:0;font-size:12px;">{{ textContent }}</pre>
      <div v-if="previewKind === 'text' && truncated" style="font-size:12px;opacity:0.6;margin-top:8px;">
        {{ t('workflow.filePreviewTruncated', { n: CSV_PREVIEW_MAX_LINES }) }}
      </div>
      <object
        v-if="previewKind === 'embed'"
        :data="fullUrl" width="100%" height="90%" :type="embedMime"
      >
        <p>{{ t('workflow.browserNotSupportEmbed') }}</p>
      </object>
      <div v-if="previewKind === 'unsupported'" style="padding: 40px 0;opacity:0.7;">
        {{ t('workflow.filePreviewUnsupported') }}
      </div>
    </div>
    <template #footer>
      <NButton type="primary" text tag="a" size="small" @click="download">
        {{ t('workflow.clickToDownload') }}{{ props.fileName }}
      </NButton>
    </template>
  </NModal>
</template>
