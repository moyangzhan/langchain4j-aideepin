<script setup lang='ts'>
import { computed, onMounted, reactive, ref } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NInput, NInputNumber, NSelect, NSpace, NSpin, useDialog, useMessage } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const dialog = useDialog()
const route = useRoute()
const router = useRouter()

const { kbUuid, docUuid } = route.params as { kbUuid: string; docUuid?: string }
const isEdit = computed(() => !!docUuid)

const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const tmpItem = reactive<KnowledgeBase.Document>(knowledgeBaseEmptyItem())
const submitting = ref<boolean>(false)
const loading = ref<boolean>(false)

// 当前已是 qa 模式且已有问答对：勾选项语义从"生成"变为"清空并重新生成"，保存时二次确认
const hasQaPairs = ref(false)

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
  // 破坏性生成在保存前需二次确认：已有问答对=清空重建；从其他模式切到 qa=丢弃现有分段
  // 并由 AI 生成（切模式时复选框是自动勾上的，用户未必意识到已开启生成）
  if (tmpItem.segmentMode === 'qa' && tmpItem.autoGenerateQa && (hasQaPairs.value || segmentModeChanged.value)) {
    dialog.warning({
      title: t('knowledgeBase.regenerateQaTitle'),
      content: hasQaPairs.value
        ? t('knowledgeBase.regenerateQaConfirm')
        : t('knowledgeBase.switchGenerateQaConfirm'),
      positiveText: t('common.confirm'),
      negativeText: t('common.cancel'),
      onPositiveClick: () => doSave(),
    })
    return
  }
  await doSave()
}

async function doSave() {
  try {
    submitting.value = true
    await api.knowledgeBaseItemSaveOrUpdate<KnowledgeBase.Document>(tmpItem)
    // qa 模式编辑正文不触发重索引（问答对与 remark 无关），成功提示按模式区分
    ms.success(tmpItem.segmentMode === 'qa' ? t('common.saveSuccess') : t('knowledgeBase.savedAndReindexing'))
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
      const resp = await api.knowledgeBaseItemInfo<KnowledgeBase.Document>(docUuid)
      Object.assign(tmpItem, resp.data)
      if (!tmpItem.segmentMode)
        tmpItem.segmentMode = 'text'
      originalSegmentMode.value = tmpItem.segmentMode
      // 仅当前即为 qa 模式时探测问答对存在性；text→qa 切换清的是文本分段，由模式切换警告覆盖
      if (tmpItem.segmentMode === 'qa') {
        const segResp = await api.documentSegmentList<{ total: number; records: unknown[] }>(docUuid, 1, 1)
        hasQaPairs.value = (segResp.data?.total ?? 0) > 0
      }
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
              {{ hasQaPairs ? t('knowledgeBase.regenerateQaLabel') : t('knowledgeBase.autoGenerateQaLabel') }}
            </NCheckbox>
          </NAlert>
          <template v-if="tmpItem.segmentMode === 'parent_child'">
            {{ t('knowledgeBase.childMaxChunkSize') }}
            <NInputNumber v-model:value="tmpItem.childMaxChunkSize" :min="50" />
          </template>
          {{ t('knowledgeBase.brief') }}
          <NInput v-model:value="tmpItem.brief" type="textarea" show-count :autosize="{ minRows: 2, maxRows: 3 }" />
          <!-- 正文与分段模式无关：remark 是文档本身的内容，qa 模式仅作为生成来源，编辑不影响已有问答对 -->
          {{ t('knowledgeBase.rawContent') }}
          <NInput
            v-model:value="tmpItem.remark"
            class="content-textarea"
            type="textarea"
            show-count
          />
          <span v-if="tmpItem.segmentMode === 'qa'" style="font-size: 12px; opacity: 0.65;">
            {{ t('knowledgeBase.rawEditQaTip') }}
          </span>
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
