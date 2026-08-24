<script setup lang='ts'>
import { computed, onMounted, reactive, ref } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCard, NIcon, NInput, NInputNumber, NRadio, NRadioGroup, NSelect, NSpin, NTooltip, useDialog, useMessage } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import { CreateOutline } from '@vicons/ionicons5'
import { useRoute, useRouter } from 'vue-router'
import { useAppStore, useKbStore } from '@/store'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import { SPLIT_STRATEGY, TOKEN_ESTIMATOR } from '@/utils/constant'
import { t } from '@/locales'
import api from '@/api'

const route = useRoute()
const router = useRouter()
const ms = useMessage()
const dialog = useDialog()
const appStore = useAppStore()
const kbStore = useKbStore()

const { kbUuid } = route.params as { kbUuid?: string }
const isEdit = computed(() => !!kbUuid)

const tmpKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const loading = ref(false)
const submitting = ref(false)
const originalTitle = ref('')
const itemBoxClass = 'space-y-1'

// 滚动区高度：仅面包屑与底部按钮固定可见，其余视口高度留给表单滚动
// Scroll height: only the breadcrumb and footer buttons stay fixed; the rest of the viewport scrolls
const settingsMaxHeight = ref<number>(500)

// 编辑打开时的切段参数快照：保存前检测变更，触发“将自动重建全库索引”确认
let kbEditOrigin: Partial<KnowledgeBase.Info> | null = null

const pageTitle = computed(() => isEdit.value ? t('knowledgeBase.editKbTitle', { title: originalTitle.value }) : t('knowledgeBase.addKbTitle'))

// 控制 input 按钮
const inputStatus = computed(() => tmpKb.title.trim().length < 1 && !submitting.value)

const SPLIT_PARAM_KEYS = ['ingestMaxSegmentSize', 'ingestMaxOverlap', 'ingestSplitStrategy', 'ingestCustomSeparator', 'ingestTokenEstimator'] as const

function splitParamsChanged() {
  if (!kbEditOrigin)
    return false
  return SPLIT_PARAM_KEYS.some(key => (tmpKb as any)[key] !== (kbEditOrigin as any)[key])
}

function applyModelDefaults() {
  if (!tmpKb.ingestModelName) {
    const firstEnableModel = appStore.llms.find((item: { enable: any }) => item.enable)
    if (firstEnableModel) {
      tmpKb.ingestModelName = firstEnableModel.modelName
      tmpKb.ingestModelId = firstEnableModel.modelId
    }
  } else {
    tmpKb.ingestModelName = appStore.llms.find(item => item.modelName === tmpKb.ingestModelName)?.modelName || ''
  }
  if (!tmpKb.ingestTokenEstimator)
    tmpKb.ingestTokenEstimator = TOKEN_ESTIMATOR[0].value
}

function onModelChange(modelName: string) {
  tmpKb.ingestModelName = modelName
  tmpKb.ingestModelId = appStore.llms.find(item => item.modelName === modelName)?.modelId || ''
}

function onTokenEstimatorChange(tokenEstimator: string) {
  tmpKb.ingestTokenEstimator = tokenEstimator
}

async function saveOrUpdateKb() {
  if (tmpKb.ingestSplitStrategy === 'custom' && !tmpKb.ingestCustomSeparator?.trim()) {
    ms.warning(t('knowledgeBase.customSeparatorRequired'))
    return
  }
  if (splitParamsChanged()) {
    dialog.warning({
      title: t('common.tip'),
      content: t('knowledgeBase.splitParamsReindexConfirm'),
      positiveText: t('common.confirm'),
      negativeText: t('common.cancel'),
      onPositiveClick: () => doSaveKb(),
    })
    return
  }
  await doSaveKb()
}

async function doSaveKb() {
  try {
    submitting.value = true
    await api.knowledgeBaseSaveOrUpdate<KnowledgeBase.Info>(tmpKb)
    kbStore.setReloadKbInfosSignal(true)
    ms.success(t('common.saveSuccess'))
    router.back()
  } finally {
    submitting.value = false
  }
}

onMounted(async () => {
  settingsMaxHeight.value = Math.max(window.innerHeight - 170, 250)
  applyModelDefaults()
  if (!isEdit.value)
    return
  loading.value = true
  try {
    const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid!)
    Object.assign(tmpKb, resp.data)
    originalTitle.value = tmpKb.title
    kbEditOrigin = { ...resp.data }
    applyModelDefaults()
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
      <NBreadcrumbItem v-if="isEdit" :href="`/#/kb-manage/${kbUuid}`">
        {{ originalTitle }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :clickable="false">
        {{ pageTitle }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NSpin :show="loading">
      <!-- Page title stays fixed above the scroll area -->
      <div class="flex items-center gap-2" style="margin-top: 12px; font-size: 16px; font-weight: 600">
        <NIcon :size="18">
          <CreateOutline />
        </NIcon>
        {{ pageTitle }}
      </div>
      <div :style="{ maxHeight: `${settingsMaxHeight}px`, overflowY: 'auto' }" class="pr-1">
        <NCard style="margin-top: 12px" hoverable>
          <div class="flex flex-col space-y-2">
            <div :class="itemBoxClass">
              <div>{{ t('common.title') }}<span class="text-red-400"> *</span></div>
              <NInput v-model:value="tmpKb.title" maxlength="100" :placeholder="t('store.title')" show-count />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('common.description') }}</div>
              <NInput
                v-model:value="tmpKb.remark" type="textarea" :placeholder="t('store.description')" maxlength="500"
                show-count :autosize="{ minRows: 3, maxRows: 10 }"
              />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.isPublic') }}</div>
              <NRadioGroup v-model:value="tmpKb.isPublic" name="radiogroup">
                <NRadio key="public_yes" :value="true">
                  {{ t('common.public') }}
                </NRadio>
                <NRadio key="public_no" :value="false">
                  {{ t('common.private') }}
                </NRadio>
              </NRadioGroup>
            </div>
            <div :class="itemBoxClass">
              <div>
                {{ t('knowledgeBase.strictMode') }}
                <NTooltip trigger="hover">
                  <template #trigger>
                    <NIcon style="padding-top: 0.1rem">
                      <QuestionCircle16Regular />
                    </NIcon>
                  </template>
                  <div>{{ t('knowledgeBase.strictModeDescShort') }}</div>
                  <div>{{ t('knowledgeBase.looseModeDescShort') }}</div>
                </NTooltip>
              </div>
              <NRadioGroup v-model:value="tmpKb.isStrict" name="radiogroup">
                <NRadio key="strict_yes" :value="true">
                  {{ t('common.yes') }}
                </NRadio>
                <NRadio key="strict_no" :value="false">
                  {{ t('common.no') }}
                </NRadio>
              </NRadioGroup>
            </div>
          </div>
        </NCard>
        <NCard style="margin-top: 12px" :title="t('knowledgeBase.docIndexSettingVector')" hoverable>
          <div class="flex flex-col space-y-2">
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.docOverlapCount') }}</div>
              <NInputNumber v-model:value="tmpKb.ingestMaxOverlap" />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.splitStrategy') }}</div>
              <NSelect v-model:value="tmpKb.ingestSplitStrategy" :options="SPLIT_STRATEGY" />
            </div>
            <div v-if="tmpKb.ingestSplitStrategy === 'custom'" :class="itemBoxClass">
              <div>{{ t('knowledgeBase.customSeparator') }}</div>
              <NInput v-model:value="tmpKb.ingestCustomSeparator" :placeholder="t('knowledgeBase.customSeparatorPlaceholder')" />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.maxSegmentSize') }}</div>
              <NInputNumber v-model:value="tmpKb.ingestMaxSegmentSize" :min="100" />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.tokenCounter') }}</div>
              <NSelect
                :value="tmpKb.ingestTokenEstimator" :options="TOKEN_ESTIMATOR"
                :on-update:value="onTokenEstimatorChange"
              />
            </div>
          </div>
        </NCard>
        <NCard style="margin-top: 12px" :title="t('knowledgeBase.docIndexSettingGraph')" hoverable>
          <div :class="itemBoxClass">
            <div>
              {{ t('knowledgeBase.modelName') }}
              <NTooltip trigger="hover">
                <template #trigger>
                  <NIcon style="padding-top: 0.1rem">
                    <QuestionCircle16Regular />
                  </NIcon>
                </template>
                <div>{{ t('knowledgeBase.modelExtractTip') }}</div>
              </NTooltip>
            </div>
            <NSelect :value="tmpKb.ingestModelName" :options="appStore.llms" :on-update:value="onModelChange" />
          </div>
        </NCard>
        <NCard style="margin-top: 12px" :title="t('knowledgeBase.docRecallSetting')" hoverable>
          <div class="flex flex-col space-y-2">
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.docRecallMaxCount') }}</div>
              <NInputNumber v-model:value="tmpKb.retrieveMaxResults" />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.docRecallMinScore') }}</div>
              <NInputNumber v-model:value="tmpKb.retrieveMinScore" :precision="1" :min="0" :max="1" />
            </div>
          </div>
        </NCard>
        <NCard style="margin-top: 12px" :title="t('knowledgeBase.llmParamSetting')" hoverable>
          <div class="flex flex-col space-y-2">
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.systemPromptRole') }}</div>
              <NInput
                v-model:value="tmpKb.querySystemMessage" type="textarea"
                :autosize="{ minRows: 2, maxRows: 5 }"
              />
            </div>
            <div :class="itemBoxClass">
              <div>{{ t('knowledgeBase.responseCreativity') }}</div>
              <NInputNumber v-model:value="tmpKb.queryLlmTemperature" :precision="1" :min="0" :max="1" />
            </div>
          </div>
        </NCard>
      </div>
      <div class="flex justify-end gap-2" style="margin-top: 12px">
        <NButton @click="router.back()">
          {{ t('common.cancel') }}
        </NButton>
        <NButton type="primary" :disabled="inputStatus" :loading="submitting" @click="saveOrUpdateKb">
          {{ t('common.confirm') }}
        </NButton>
      </div>
    </NSpin>
  </div>
</template>
