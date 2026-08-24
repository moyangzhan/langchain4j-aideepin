<script setup lang='ts'>
import { computed, onMounted, reactive, ref } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NInput, NInputNumber, NSelect, NSpace, NSpin, useMessage } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const route = useRoute()
const router = useRouter()

const { kbUuid, docUuid } = route.params as { kbUuid: string; docUuid?: string }
const isEdit = computed(() => !!docUuid)

const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const tmpItem = reactive<KnowledgeBase.Item>(knowledgeBaseEmptyItem())
const submitting = ref<boolean>(false)
const loading = ref<boolean>(false)

const segmentModeOptions = [
  { label: t('knowledgeBase.segmentModeText'), value: 'text' },
  { label: t('knowledgeBase.segmentModeQa'), value: 'qa' },
  { label: t('knowledgeBase.segmentModeParentChild'), value: 'parent_child' },
]

// 编辑已有文档时记录加载时的分段模式，用于检测用户是否切换了模式
// Remember the segment mode loaded from the server to detect a user switch on existing docs
const originalSegmentMode = ref<string>('')
const segmentModeChanged = computed(() => isEdit.value && !!originalSegmentMode.value && tmpItem.segmentMode !== originalSegmentMode.value)

// Switching to qa mode defaults auto-generation on; switching away clears it.
// Loading an existing qa doc keeps the checkbox off (fill-later is opt-in).
function onSegmentModeChange(val: string) {
  tmpItem.segmentMode = val
  tmpItem.autoGenerateQa = val === 'qa'
}

const pageTitle = computed(() => {
  return isEdit.value
    ? t('knowledgeBase.knowledgeItemEdit', { title: tmpItem.title })
    : t('knowledgeBase.knowledgeItemAdd')
})

// 控制 input 按钮
const inputStatus = computed(() => tmpItem.title.trim().length < 1 && !submitting.value)

async function saveOrUpdate() {
  try {
    submitting.value = true
    await api.knowledgeBaseItemSaveOrUpdate<KnowledgeBase.Item>(tmpItem)
    ms.success(t('knowledgeBase.savedAndReindexing'))
    router.back()
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    submitting.value = false
  }
}

onMounted(async () => {
  loading.value = true
  try {
    const kbResp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid)
    Object.assign(curKb, kbResp.data)
    if (isEdit.value && docUuid) {
      const resp = await api.knowledgeBaseItemInfo<KnowledgeBase.Item>(docUuid)
      Object.assign(tmpItem, resp.data)
      if (!tmpItem.segmentMode)
        tmpItem.segmentMode = 'text'
      originalSegmentMode.value = tmpItem.segmentMode
    } else {
      tmpItem.kbId = curKb.id
      tmpItem.kbUuid = kbUuid
      tmpItem.segmentMode = 'text'
    }
  } finally {
    loading.value = false
  }
})
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
        {{ pageTitle }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NCard style="margin-top: 12px" :title="pageTitle" hoverable>
      <NSpin :show="loading">
        <NSpace vertical>
          {{ t('store.title') }}
          <NInput v-model:value="tmpItem.title" maxlength="100" show-count />
          {{ t('knowledgeBase.segmentMode') }}
          <NSelect :value="tmpItem.segmentMode" :options="segmentModeOptions" :on-update:value="onSegmentModeChange" />
          <div style="font-size: 12px; opacity: 0.7" class="flex flex-col">
            <div>{{ t('knowledgeBase.segmentModeTextTip') }}</div>
            <div>{{ t('knowledgeBase.segmentModeQaTip') }}</div>
            <div>{{ t('knowledgeBase.segmentModeParentChildTip') }}</div>
          </div>
          <NAlert
            v-if="segmentModeChanged || tmpItem.segmentMode === 'qa'"
            type="warning"
            :show-icon="true"
            style="font-size: 12px"
          >
            <div v-if="segmentModeChanged">
              {{ t('knowledgeBase.segmentModeTip') }}
            </div>
            <div v-if="tmpItem.segmentMode === 'qa' && !tmpItem.autoGenerateQa">
              {{ t('knowledgeBase.segmentModeSwitchQaWarning') }}
            </div>
            <NCheckbox
              v-if="tmpItem.segmentMode === 'qa'"
              v-model:checked="tmpItem.autoGenerateQa"
              style="margin-top: 4px"
            >
              {{ t('knowledgeBase.autoGenerateQaLabel') }}
            </NCheckbox>
          </NAlert>
          <template v-if="tmpItem.segmentMode === 'parent_child'">
            {{ t('knowledgeBase.childMaxChunkSize') }}
            <NInputNumber v-model:value="tmpItem.childMaxChunkSize" :min="50" />
          </template>
          {{ t('knowledgeBase.brief') }}
          <NInput v-model:value="tmpItem.brief" type="textarea" show-count :autosize="{ minRows: 2, maxRows: 3 }" />
          <template v-if="tmpItem.segmentMode !== 'qa'">
            {{ t('common.content') }}
            <NInput
              v-model:value="tmpItem.remark"
              class="content-textarea"
              type="textarea"
              show-count
            />
          </template>
        </NSpace>
      </NSpin>
      <template #footer>
        <div class="flex justify-end gap-2">
          <NButton @click="router.back()">
            {{ t('common.cancel') }}
          </NButton>
          <NButton type="primary" :disabled="inputStatus" :loading="submitting" @click="saveOrUpdate">
            {{ t('common.confirm') }}
          </NButton>
        </div>
      </template>
    </NCard>
  </div>
</template>

<style scoped>
.content-textarea :deep(textarea) {
  height: calc(100vh - 480px) !important;
  resize: none;
}
</style>
