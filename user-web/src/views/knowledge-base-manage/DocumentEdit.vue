<script setup lang='ts'>
import { computed, h, onActivated, onMounted, reactive, ref } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NInput, NInputNumber, NSelect, NSpace, NSpin, useDialog, useMessage, useThemeVars } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'
import { useKbStore } from '@/store'
import { effectiveIngest, knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { SPLIT_STRATEGY } from '@/utils/constant'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const dialog = useDialog()
const themeVars = useThemeVars()
const kbStore = useKbStore()
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

const effective = computed(() => effectiveIngest(tmpItem, curKb))

// 编辑框始终显示生效值（未覆盖=知识库默认，预填便于增量修改）；真实改动才写覆盖，清空回哨兵
// Editors always show the effective value (KB default when not overridden, prefilled for incremental edits); only real changes write the override, clearing returns to the sentinel
const strategyValue = computed<string>({
  get: () => effective.value.strategy,
  set: (v) => {
    tmpItem.ingestSplitStrategy = v ?? ''
  },
})

const customSeparatorValue = computed<string>({
  get: () => effective.value.customSeparator,
  set: (v) => {
    tmpItem.ingestCustomSeparator = v ?? ''
  },
})

const maxSegmentSizeValue = computed<number | null>({
  get: () => effective.value.maxSegmentSize,
  set: (v) => {
    tmpItem.ingestMaxSegmentSize = v ?? -1
  },
})

const maxOverlapSizeValue = computed<number | null>({
  get: () => effective.value.maxOverlapSize,
  set: (v) => {
    tmpItem.ingestMaxOverlapSize = v ?? -1
  },
})

const overridden = computed(() => ({
  strategy: !!tmpItem.ingestSplitStrategy,
  maxSegmentSize: (tmpItem.ingestMaxSegmentSize ?? -1) >= 0,
  maxOverlapSize: (tmpItem.ingestMaxOverlapSize ?? -1) >= 0,
  customSeparator: !!tmpItem.ingestCustomSeparator,
}))

type OverrideKey = keyof typeof overridden.value

// 单值点击编辑：未覆盖显示生效值，点击进入编辑；覆盖后编辑框常驻并露出「恢复」按钮
// Single-value click-to-edit: the effective value shows when not overridden; once overridden the editor stays with a Reset button
const expanded = ref(new Set<OverrideKey>())

function editorOpen(key: OverrideKey): boolean {
  return overridden.value[key] || expanded.value.has(key)
}

function startEdit(key: OverrideKey) {
  expanded.value.add(key)
}

function collapseIfClean(key: OverrideKey) {
  if (!overridden.value[key])
    expanded.value.delete(key)
}

function resetOverride(key: OverrideKey) {
  if (key === 'strategy')
    tmpItem.ingestSplitStrategy = ''
  else if (key === 'customSeparator')
    tmpItem.ingestCustomSeparator = ''
  else if (key === 'maxSegmentSize')
    tmpItem.ingestMaxSegmentSize = -1
  else
    tmpItem.ingestMaxOverlapSize = -1
  expanded.value.delete(key)
}

function strategyLabel(value?: string): string {
  return SPLIT_STRATEGY.find(option => option.value === value)?.label ?? value ?? ''
}

const tokenLabel = computed(() => tmpItem.segmentMode === 'parent_child'
  ? t('knowledgeBase.parentChunkMaxLabel')
  : tmpItem.segmentMode === 'qa' ? t('knowledgeBase.qaChunkMaxLabel') : t('knowledgeBase.textChunkMaxLabel'))

// overlap 上限随生效片段大小联动，拦截口径与库级表单一致
// Overlap upper bound follows the effective segment size, matching the KB-level form
const overlapMax = computed(() => Math.max((effective.value.maxSegmentSize ?? 1000) - 1, 0))

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
  // 覆盖值与库默认合并后的拦截口径与库级表单一致（输入 max 已拦，此处兜底手输越界值）
  // Validation after merging overrides with KB defaults, matching the KB form (input max blocks normal input, this backstops escaped values)
  const eff = effectiveIngest(tmpItem, curKb)
  if (eff.strategy === 'custom' && !eff.customSeparator?.trim()) {
    ms.warning(t('knowledgeBase.customSeparatorRequired'))
    return
  }
  const effSize = eff.maxSegmentSize ?? 0
  if (effSize > 0 && (eff.maxOverlapSize ?? 0) >= effSize) {
    ms.warning(t('knowledgeBase.overlapTooLarge'))
    return
  }
  // 破坏性生成在保存前需二次确认：已有问答对=清空重建；从其他模式切到 qa=丢弃现有分段
  // 并由 AI 生成（切模式时复选框是自动勾上的，用户未必意识到已开启生成）
  if (tmpItem.segmentMode === 'qa' && tmpItem.autoGenerateQa && (hasQaPairs.value || segmentModeChanged.value)) {
    dialog.warning({
      title: t('knowledgeBase.regenerateQaTitle'),
      content: () => h('div', null, [
        h('div', hasQaPairs.value
          ? t('knowledgeBase.regenerateQaConfirm')
          : t('knowledgeBase.switchGenerateQaConfirm')),
        h('div', { style: 'margin-top: 6px' }, t('knowledgeBase.generateQaModelLine', { model: curKb.ingestModelName || '-' })),
      ]),
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
    const resp = await api.knowledgeBaseItemSaveOrUpdate<KnowledgeBase.Document>(tmpItem)
    kbStore.markDocsChanged(kbUuid)
    // qa+generation saves dispatch an async QA generation task instead of re-embedding
    ms.success(tmpItem.segmentMode === 'qa'
      ? (tmpItem.autoGenerateQa ? t('knowledgeBase.savedAndGenerating') : t('common.saveSuccess'))
      : t('knowledgeBase.savedAndReindexing'))
    // A newly created Q&A doc opens its detail page; edits keep the back navigation
    if (!isEdit.value && tmpItem.segmentMode === 'qa' && resp.data?.uuid)
      router.replace({ name: 'DocumentDetail', params: { kbUuid, docUuid: resp.data.uuid } })
    else
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

// re-activation only refreshes the KB info (may have changed via KnowledgeBaseEdit);
// tmpItem is untouched so unsaved edits survive
let activateCount = 0
onActivated(async () => {
  activateCount++
  // first activation follows onMounted, which already loaded the KB info
  if (activateCount === 1)
    return
  const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid)
  Object.assign(curKb, resp.data)
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
          <span style="font-weight: 600;">{{ t('store.title') }}</span>
          <NInput v-model:value="tmpItem.title" maxlength="100" show-count />
          <span style="font-weight: 600;">{{ t('knowledgeBase.segmentMode') }}</span>
          <NCard size="small">
            <NSpace vertical>
              <NSelect :value="tmpItem.segmentMode" :options="segmentModeOptions" :on-update:value="onSegmentModeChange" />
              <div style="font-size: 12px; opacity: 0.7" class="flex flex-col">
                <div>{{ t('knowledgeBase.segmentModeTextTip') }}</div>
                <div>{{ t('knowledgeBase.segmentModeQaTip') }}</div>
                <div>{{ t('knowledgeBase.segmentModeParentChildTip') }}</div>
              </div>
              <!-- 切换模式的破坏性后果：常显、warning 黄加粗，紧跟模式说明 -->
              <div style="font-size: 12px; font-weight: 600; color: #f0a020;">
                {{ t('knowledgeBase.segmentModeTip') }}
              </div>
              <NCheckbox
                v-if="tmpItem.segmentMode === 'qa'"
                v-model:checked="tmpItem.autoGenerateQa"
              >
                {{ hasQaPairs ? t('knowledgeBase.regenerateQaLabel') : t('knowledgeBase.autoGenerateQaLabel') }}
              </NCheckbox>
            </NSpace>
          </NCard>
          <!-- qa 模式仅勾选 AI 生成时展示：切分参数只在生成路径生效 -->
          <template v-if="tmpItem.segmentMode !== 'qa' || tmpItem.autoGenerateQa">
            <span style="font-weight: 600;">{{ t('knowledgeBase.segmentConfigLabel') }}</span>
            <NCard size="small">
              <div class="seg-grid" style="font-size: 12px;">
                <template v-if="tmpItem.segmentMode === 'qa'">
                  <span>{{ t('knowledgeBase.ingestModelLabel') }}:</span>
                  <div class="flex items-center gap-2 flex-wrap">
                    <span>{{ curKb.ingestModelName || '-' }}</span>
                    <span style="opacity: 0.6;">
                      {{ t('knowledgeBase.modelChangePrefix') }}<RouterLink class="click-link" :style="{ color: themeVars.primaryColor }" :to="{ name: 'KnowledgeBaseEdit', params: { kbUuid }, query: { anchor: 'ingestModelCard' } }">{{ t('knowledgeBase.kbSettingsLink') }}</RouterLink>{{ t('knowledgeBase.modelChangeSuffix') }}
                    </span>
                  </div>
                </template>
                <span class="span-all" style="opacity: 0.7;">{{ t('knowledgeBase.docOverrideTip') }}</span>
                <span>{{ tokenLabel }}:</span>
                <div class="flex items-center gap-2 flex-wrap">
                  <span v-if="!editorOpen('maxSegmentSize')" class="click-edit" :style="{ color: themeVars.primaryColor }" @click="startEdit('maxSegmentSize')">{{ effective.maxSegmentSize ?? 0 }}</span>
                  <template v-else>
                    <NInputNumber
                      v-model:value="maxSegmentSizeValue" :min="100" :max="10000" size="small"
                      :placeholder="t('knowledgeBase.followKbPlaceholder')" style="width: 180px"
                      @blur="collapseIfClean('maxSegmentSize')"
                    />
                    <NButton v-if="overridden.maxSegmentSize" size="tiny" quaternary type="primary" @click="resetOverride('maxSegmentSize')">
                      {{ t('knowledgeBase.resetOverride') }}
                    </NButton>
                  </template>
                </div>
                <span>{{ t('knowledgeBase.configOverlapLabel') }}:</span>
                <div class="flex items-center gap-2 flex-wrap">
                  <span v-if="!editorOpen('maxOverlapSize')" class="click-edit" :style="{ color: themeVars.primaryColor }" @click="startEdit('maxOverlapSize')">{{ effective.maxOverlapSize ?? 0 }}</span>
                  <template v-else>
                    <NInputNumber
                      v-model:value="maxOverlapSizeValue" :min="0" :max="overlapMax" size="small"
                      :placeholder="t('knowledgeBase.followKbPlaceholder')" style="width: 180px"
                      @blur="collapseIfClean('maxOverlapSize')"
                    />
                    <NButton v-if="overridden.maxOverlapSize" size="tiny" quaternary type="primary" @click="resetOverride('maxOverlapSize')">
                      {{ t('knowledgeBase.resetOverride') }}
                    </NButton>
                  </template>
                </div>
                <span>{{ t('knowledgeBase.configStrategyLabel') }}:</span>
                <div class="flex items-center gap-2 flex-wrap">
                  <span v-if="!editorOpen('strategy')" class="click-edit" :style="{ color: themeVars.primaryColor }" @click="startEdit('strategy')">{{ strategyLabel(effective.strategy) }}</span>
                  <template v-else>
                    <NSelect
                      v-model:value="strategyValue" :options="SPLIT_STRATEGY" size="small"
                      :placeholder="t('knowledgeBase.followKbPlaceholder')" style="width: 180px"
                      @blur="collapseIfClean('strategy')"
                    />
                    <NButton v-if="overridden.strategy" size="tiny" quaternary type="primary" @click="resetOverride('strategy')">
                      {{ t('knowledgeBase.resetOverride') }}
                    </NButton>
                  </template>
                </div>
                <template v-if="effective.strategy === 'custom'">
                  <span>{{ t('knowledgeBase.customSeparator') }}:</span>
                  <div class="flex items-center gap-2 flex-wrap">
                    <span v-if="!editorOpen('customSeparator')" class="click-edit" :style="{ color: themeVars.primaryColor }" @click="startEdit('customSeparator')">{{ effective.customSeparator || '-' }}</span>
                    <template v-else>
                      <NInput
                        v-model:value="customSeparatorValue" size="small"
                        :placeholder="t('knowledgeBase.followKbPlaceholder')" style="width: 180px"
                        @blur="collapseIfClean('customSeparator')"
                      />
                      <NButton v-if="overridden.customSeparator" size="tiny" quaternary type="primary" @click="resetOverride('customSeparator')">
                        {{ t('knowledgeBase.resetOverride') }}
                      </NButton>
                    </template>
                  </div>
                </template>
                <template v-if="tmpItem.segmentMode === 'parent_child'">
                  <span>{{ t('knowledgeBase.childChunkMaxLabel') }}:</span>
                  <div class="flex items-center gap-2">
                    <NInputNumber v-model:value="tmpItem.childMaxChunkSize" :min="50" :max="4000" size="small" style="width: 180px" />
                  </div>
                </template>
                <span v-if="tmpItem.segmentMode === 'qa'" class="span-all" style="opacity: 0.7;">{{ t('knowledgeBase.qaNoChunkRow') }}</span>
              </div>
            </NCard>
          </template>
          <span style="font-weight: 600;">{{ t('knowledgeBase.brief') }}</span>
          <NInput v-model:value="tmpItem.brief" type="textarea" show-count :autosize="{ minRows: 2, maxRows: 3 }" />
          <span style="font-weight: 600;">{{ t('knowledgeBase.rawContent') }}</span>
          <NInput
            v-model:value="tmpItem.remark"
            class="content-textarea"
            type="textarea"
            show-count
          />
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

/* 值列固定为剩余宽度：编辑框出现时内容换行而非撑宽轨道，避免页面横向抖动
   Value column pinned to the remaining width: editors wrap instead of widening the track */
.seg-grid {
  display: grid;
  grid-template-columns: max-content minmax(0, 1fr);
  align-items: center;
  row-gap: 4px;
  column-gap: 12px;
}

.seg-grid .span-all {
  grid-column: 1 / -1;
}

/* 与点击编辑值同款可点文字样式（主题色+hover 虚线下划），hljs-link 是代码高亮主题色不跟主题
   Same clickable style as click-to-edit values (theme color + dotted hover); hljs-link is a fixed code-highlight color */
.click-edit,
.click-link {
  cursor: pointer;
}

.click-edit:hover,
.click-link:hover {
  text-decoration: underline dotted;
}
</style>
