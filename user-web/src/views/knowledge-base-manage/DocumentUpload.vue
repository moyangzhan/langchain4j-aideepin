<script setup lang='ts'>
import { computed, onActivated, onDeactivated, onUnmounted, reactive, ref, watch } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NFlex, NIcon, NInputNumber, NP, NSelect, NSpace, NSpin, NText, NUpload, NUploadDragger, useMessage } from 'naive-ui'
import { ArchiveOutline } from '@vicons/ionicons5'
import { useRoute, useRouter } from 'vue-router'
import type { UploadFileInfo, UploadInst } from 'naive-ui'
import SegmentConfigPanel from './components/SegmentConfigPanel.vue'
import QaImportFormatAlert from './components/QaImportFormatAlert.vue'
import { useAuthStore } from '@/store'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const route = useRoute()
const router = useRouter()
const { kbUuid } = route.params as { kbUuid: string }

// true until the first activation finishes loading
const loading = ref(true)
const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())

const authStore = useAuthStore()
const headers = computed(() => ({ Authorization: authStore.token }))
const uploadRef = ref<UploadInst | null>(null)
const fileList = ref<UploadFileInfo[]>([])

const uploadSegmentMode = ref<string>('text')
const uploadChildMaxChunkSize = ref<number | null>(200)
const segmentModeOptions = [
  { label: t('knowledgeBase.segmentModeText'), value: 'text' },
  { label: t('knowledgeBase.segmentModeQa'), value: 'qa' },
  { label: t('knowledgeBase.segmentModeParentChild'), value: 'parent_child' },
]
const uploadAction = computed(() => {
  let url = `/api/knowledge-base/upload/${kbUuid}?segmentMode=${uploadSegmentMode.value}`
  if (uploadSegmentMode.value === 'parent_child' && uploadChildMaxChunkSize.value)
    url += `&childMaxChunkSize=${uploadChildMaxChunkSize.value}`
  return url
})
const uploadModeTip = computed(() => ({
  text: t('knowledgeBase.segmentModeTextTip'),
  qa: t('knowledgeBase.segmentModeQaTip'),
  parent_child: t('knowledgeBase.segmentModeParentChildTip'),
}[uploadSegmentMode.value] ?? ''))
watch(uploadSegmentMode, () => {
  fileList.value = []
  resetQaUploadOutcome()
})
const fileListLength = computed(() => fileList.value.length)

async function downloadQaTemplate() {
  try {
    const resp = await api.downloadQaImportTemplate()
    const url = URL.createObjectURL(resp.data)
    const link = document.createElement('a')
    link.href = url
    link.download = 'qa_import_template.csv'
    link.click()
    // revoke after the download has started: immediate revocation can abort it in some browsers
    setTimeout(() => URL.revokeObjectURL(url), 0)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

const qaFailures = ref<{ name: string; reason: string }[]>([])
const qaSucceededCount = ref(0)

function resetQaUploadOutcome() {
  qaFailures.value = []
  qaSucceededCount.value = 0
}

function recordQaFailure(file: UploadFileInfo, reason: string) {
  qaFailures.value.push({ name: file.name, reason })
}

// stays true when any file in the current submit failed: keeps the page open
let uploadHasError = false

function markUploadFailed(file: UploadFileInfo) {
  const row = fileList.value.find(f => f.id === file.id)
  if (row)
    row.status = 'error'
}

function onUploadError({ file }: { file: UploadFileInfo; event?: ProgressEvent }) {
  uploadHasError = true
  markUploadFailed(file)
  if (uploadSegmentMode.value === 'qa')
    recordQaFailure(file, t('common.uploadFailed'))
  else
    ms.error(t('common.uploadFailed'))
}

// true from submit until every file settles (uploaded / failed)
const uploadSubmitting = ref(false)

// generation token of the current page stay: deactivate/unmount (the route view is
// KeepAlive-cached) bumps it, so a poll started before leaving dies instead of firing
// router.back() after the user moved on
let stayToken = 0
onDeactivated(() => {
  stayToken++
})
onUnmounted(() => {
  stayToken++
})

// the cached instance outlives visits to KnowledgeBaseEdit: reset the state and reload
// the KB info on every activation, or a stale config display would diverge from how
// files actually get split
onActivated(async () => {
  fileList.value = []
  uploadSegmentMode.value = 'text'
  uploadChildMaxChunkSize.value = 200
  uploadSubmitting.value = false
  resetQaUploadOutcome()
  loading.value = true
  try {
    const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid)
    Object.assign(curKb, resp.data)
  } finally {
    loading.value = false
  }
})

function onUploadSubmit() {
  if (uploadSubmitting.value)
    return
  uploadHasError = false
  resetQaUploadOutcome()
  if (uploadSegmentMode.value === 'qa') {
    fileList.value.forEach((f) => {
      if (f.status === 'error')
        f.status = 'pending'
    })
  }
  uploadSubmitting.value = true
  uploadRef.value?.submit()
  backWhenUploadDone(stayToken)
}

function backWhenUploadDone(token: number) {
  setTimeout(() => {
    if (token !== stayToken)
      return
    const busy = fileList.value.some(f => f.status === 'pending' || f.status === 'uploading')
    if (busy) {
      backWhenUploadDone(token)
      return
    }
    uploadSubmitting.value = false
    if (!uploadHasError)
      router.back()
  }, 1000)
}

function onUploadFinish({
  file,
  event,
}: {
  file: UploadFileInfo
  event?: ProgressEvent
}) {
  // a non-JSON body would leave the row stuck busy forever
  let respData: { success?: boolean; message?: string } | null = null
  try {
    respData = JSON.parse((event?.target as XMLHttpRequest).response)
  } catch {
    respData = null
  }
  const qaMode = uploadSegmentMode.value === 'qa'
  if (!respData) {
    uploadHasError = true
    markUploadFailed(file)
    if (qaMode)
      recordQaFailure(file, t('knowledgeBase.uploadFailedResponseError'))
    else
      ms.error(t('knowledgeBase.uploadFailedResponseError'))

    return file
  }
  const { success, message } = respData
  if (success) {
    if (qaMode) {
      qaSucceededCount.value++
      fileList.value = fileList.value.filter(f => f.id !== file.id)
    } else {
      ms.success(t('common.uploadSuccess'))
    }
  } else {
    uploadHasError = true
    markUploadFailed(file)
    if (qaMode)
      recordQaFailure(file, message || t('common.uploadFailed'))
    else
      ms.error(message || t('common.uploadFailed'))
  }

  return file
}
</script>

<template>
  <div class="p-4">
    <NBreadcrumb separator=">">
      <NBreadcrumbItem href="/">
        {{ t('common.home') }}
      </NBreadcrumbItem>
      <NBreadcrumbItem href="/#/kb-manage">
        {{ t('knowledgeBase.myKnowledgeBase') }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :href="`/#/kb-manage/${kbUuid}`">
        {{ curKb.title }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :clickable="false">
        {{ t('knowledgeBase.knowledgeItemUpload') }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.uploadDocToGenerate')" hoverable>
      <NSpin :show="loading">
        <NSpace vertical>
          <span style="font-weight: 600;">{{ t('knowledgeBase.segmentMode') }}</span>
          <NSelect v-model:value="uploadSegmentMode" :options="segmentModeOptions" :disabled="uploadSubmitting" />
          <div style="font-size: 12px; opacity: 0.7">
            {{ uploadModeTip }}
          </div>
          <SegmentConfigPanel :kb="curKb" :kb-uuid="kbUuid" :segment-mode="uploadSegmentMode">
            <div v-if="uploadSegmentMode === 'parent_child'" class="flex items-center gap-2">
              <span>{{ t('knowledgeBase.childChunkMaxLabel') }}:</span>
              <NInputNumber v-model:value="uploadChildMaxChunkSize" :min="50" :max="4000" size="small" style="width: 140px" />
            </div>
          </SegmentConfigPanel>
          <template v-if="uploadSegmentMode === 'qa'">
            <div>
              <NButton type="primary" ghost size="small" @click="downloadQaTemplate">
                {{ t('knowledgeBase.downloadTemplate') }}
              </NButton>
            </div>
            <QaImportFormatAlert />
          </template>
          <NUpload
            ref="uploadRef" v-model:file-list="fileList" multiple directory-dnd
            :action="uploadAction"
            :default-upload="false" :max="20" :headers="headers" @finish="onUploadFinish"
            @error="onUploadError"
          >
            <NUploadDragger>
              <div style="margin-bottom: 12px">
                <NIcon size="48" :depth="3">
                  <ArchiveOutline />
                </NIcon>
              </div>
              <NText style="font-size: 16px">
                {{ t('knowledgeBase.clickOrDragToUpload') }}
              </NText>
              <NP depth="3" style="margin: 8px 0 0 0">
                <template v-if="uploadSegmentMode === 'qa'">
                  {{ t('knowledgeBase.qaUploadFileHint') }}<br>
                </template>
                <template v-else>
                  {{ t('knowledgeBase.supportedFileFormats') }}<br>
                </template>
                {{ t('knowledgeBase.fileSizeLimit') }}
              </NP>
            </NUploadDragger>
          </NUpload>
          <NAlert
            v-if="uploadSegmentMode === 'qa' && qaFailures.length"
            type="warning" :show-icon="false" style="font-size: 12px;"
          >
            <div style="font-weight: 600;">
              {{ t('knowledgeBase.qaUploadSummaryTitle', { success: qaSucceededCount, fail: qaFailures.length }) }}
            </div>
            <div v-for="(failure, index) in qaFailures" :key="index">
              {{ failure.name }} — {{ failure.reason }}
            </div>
          </NAlert>
          <NFlex>
            <NButton type="primary" :disabled="!fileListLength" :loading="uploadSubmitting" @click="onUploadSubmit">
              {{ t('knowledgeBase.uploadAndGenerate') }}
            </NButton>
            <NButton @click="router.back()">
              {{ t('common.cancel') }}
            </NButton>
          </NFlex>
        </NSpace>
      </NSpin>
    </NCard>
  </div>
</template>
