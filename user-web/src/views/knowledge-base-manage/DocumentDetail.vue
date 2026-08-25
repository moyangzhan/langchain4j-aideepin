<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NDataTable, NIcon, NInput, NModal, NP, NSpace, NSpin, NSwitch, NText, NTooltip, NUpload, NUploadDragger, useDialog, useMessage } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import { computed, h, onMounted, onUnmounted, reactive, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useAuthStore } from '@/store'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const route = useRoute()
const router = useRouter()
const ms = useMessage()
const dialog = useDialog()
const authStore = useAuthStore()

const { kbUuid } = route.params as { kbUuid: string; docUuid: string }
const curDocUuid = ref<string>('')

const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const curDoc = reactive<KnowledgeBase.Item>(knowledgeBaseEmptyItem())
const docLoading = ref(false)

const tableMaxHeight = ref<number>(400)

const segments = ref<KnowledgeBase.Segment[]>([])
const loading = ref(false)
// 模式由返回结构推断：questions 有值为 qa，children 有值为 parent_child
const segmentMode = ref<'text' | 'qa' | 'parent_child'>('text')
const paginationReactive = reactive({
  page: 1,
  pageSize: 20,
  itemCount: 0,
  prefix: () => t('common.total', { n: paginationReactive.itemCount }),
})

// 编辑/新增弹窗状态：type 决定调用哪个保存接口
const editState = reactive<{
  show: boolean
  type: 'segment' | 'question' | 'child'
  id?: string
  docUuid: string
  answerSegmentId?: string
  parentSegmentId?: string
  content: string
  answerContent?: string
  isNew: boolean
}>({
  show: false,
  type: 'segment',
  docUuid: '',
  content: '',
  isNew: false,
})
const submitting = ref(false)

const embeddingStatusLabel = computed(() => {
  switch (curDoc.embeddingStatus) {
    case 'DOING':
      return t('knowledgeBase.statusProcessing')
    case 'DONE':
      return t('knowledgeBase.statusVectorized')
    case 'FAIL':
      return t('knowledgeBase.statusFailed')
    default:
      return t('knowledgeBase.statusPending')
  }
})

function editTitle() {
  const prefix = editState.isNew
    ? (editState.type === 'question' ? t('knowledgeBase.qaQuestion') : t('knowledgeBase.childChunks'))
    : t('common.edit')
  return prefix
}

async function loadDocInfo(docUuid: string) {
  docLoading.value = true
  try {
    const [kbResp, docResp] = await Promise.all([
      api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid),
      api.knowledgeBaseItemInfo<KnowledgeBase.Item>(docUuid),
    ])
    Object.assign(curKb, kbResp.data)
    Object.assign(curDoc, docResp.data)
    // 列表为空时（如新建的 QA 文档）用文档自身的分段模式初始化，保证空态下也能新增
    if (segments.value.length === 0 && curDoc.segmentMode)
      segmentMode.value = curDoc.segmentMode as 'text' | 'qa' | 'parent_child'
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    docLoading.value = false
  }
}

// 启用分段的重建是异步的：重建中禁用开关防竞态；失败可点击标签重试（幂等）
function isRebuilding(row: KnowledgeBase.Segment) {
  return row.embeddingStatus === 'DOING' || row.graphicalStatus === 'DOING'
}

function isRebuildFailed(row: KnowledgeBase.Segment) {
  return row.isEnabled !== false && (row.embeddingStatus === 'FAIL' || row.graphicalStatus === 'FAIL')
}

function retryRebuild(row: KnowledgeBase.Segment) {
  confirmToggleStatus(row, true)
}

// （重新）生成问答对：空=直接生成；已有数据=确认后替换式重新生成（服务端守卫在跑/生成中）
function confirmGenerateQa() {
  if (paginationReactive.itemCount > 0) {
    dialog.warning({
      title: t('knowledgeBase.regenerateQaTitle'),
      content: t('knowledgeBase.regenerateQaConfirm'),
      positiveText: t('common.confirm'),
      negativeText: t('common.cancel'),
      onPositiveClick: () => doGenerateQa(),
    })
    return
  }
  doGenerateQa()
}

async function doGenerateQa() {
  try {
    await api.knowledgeBaseItemAutoGenerateQa(curDocUuid.value)
    await loadDocInfo(curDocUuid.value)
    await loadList(1)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

// 导入问答对到当前文档（追加语义，相同答案并入既有段）：直传后端解析，成功后刷新
const showQaImportModal = ref(false)
const qaImportHeaders = { Authorization: '' }

watch(() => authStore.token, (val) => {
  if (val)
    qaImportHeaders.Authorization = val
}, { immediate: true })

function downloadQaTemplate() {
  window.open('/api/document/qaImportTemplate')
}

function onQaImportFinish({ event }: { event?: ProgressEvent }) {
  showQaImportModal.value = false
  try {
    const resp = JSON.parse((event?.target as XMLHttpRequest)?.responseText || '{}')
    if (resp.success) {
      ms.success(t('common.uploadSuccess'))
      loadDocInfo(curDocUuid.value)
      loadList(1)
    }
    else {
      ms.error(resp.message || 'error')
    }
  }
  catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

// 文档级失败重试：入口只看状态列（FAIL），是否展示原因文本与之解耦
async function retryDocIndex() {
  try {
    await api.knowledgeBaseItemRetryIndex(curDocUuid.value)
    await loadDocInfo(curDocUuid.value)
    await loadList(paginationReactive.page)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

// 正文弹窗：只读查看；编辑跳转文档编辑页——正文编辑联动标题/摘要/模式切换/自动生成
// 等一系列文档级语义，统一在编辑页完成，不在详情页复刻 saveOrUpdate 的子集
const showRawContent = ref(false)

function goEditDocument() {
  showRawContent.value = false
  router.push(`/kb-manage/${kbUuid}/document/${curDocUuid.value}`)
}

async function loadList(currentPage: number) {
  loading.value = true
  try {
    const resp = await api.documentSegmentList<PageResponse>(curDocUuid.value, currentPage, paginationReactive.pageSize)
    segments.value = resp.data.records
    if (segments.value.length > 0) {
      if (segments.value[0].questions)
        segmentMode.value = 'qa'
      else if (segments.value[0].children)
        segmentMode.value = 'parent_child'
      else
        segmentMode.value = 'text'
    }
    paginationReactive.page = currentPage
    paginationReactive.itemCount = resp.data.total
    // 删除末页最后一条后回退到第一页，避免停留在空页
    if (segments.value.length === 0 && currentPage > 1) {
      await loadList(1)
      return
    }
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    loading.value = false
    scheduleAutoRefresh()
  }
}

// 存在重建中的段时定时刷新列表，直至全部完成
let refreshTimer: ReturnType<typeof setTimeout> | null = null
function scheduleAutoRefresh() {
  if (refreshTimer)
    clearTimeout(refreshTimer)
  if (segments.value.some(s => isRebuilding(s)))
    refreshTimer = setTimeout(() => loadList(paginationReactive.page), 3000)
}

function openEdit(type: 'segment' | 'question' | 'child', row: any) {
  Object.assign(editState, {
    show: true,
    type,
    id: row.id,
    docUuid: curDocUuid.value,
    answerSegmentId: row.answerSegmentId,
    parentSegmentId: row.parentSegmentId,
    content: row.content,
    isNew: false,
  })
}

function openAddQaPair() {
  Object.assign(editState, {
    show: true,
    type: 'question',
    id: undefined,
    docUuid: curDocUuid.value,
    answerSegmentId: undefined,
    answerContent: '',
    content: '',
    isNew: true,
  })
}

function openAddQuestion(answerSegmentId: string) {
  Object.assign(editState, {
    show: true,
    type: 'question',
    id: undefined,
    docUuid: curDocUuid.value,
    answerSegmentId,
    content: '',
    isNew: true,
  })
}

function openAddChild(parentSegmentId: string) {
  Object.assign(editState, {
    show: true,
    type: 'child',
    id: undefined,
    docUuid: curDocUuid.value,
    parentSegmentId,
    content: '',
    isNew: true,
  })
}

async function saveEdit() {
  if (!editState.content.trim()) {
    ms.warning(t('common.inputPlaceholder'))
    return
  }
  try {
    submitting.value = true
    if (editState.type === 'segment') {
      await api.documentSegmentSaveOrUpdate({ id: editState.id!, docUuid: editState.docUuid, content: editState.content } as KnowledgeBase.Segment)
    } else if (editState.type === 'question') {
      await api.documentSegmentQuestionSaveOrUpdate({
        id: editState.id,
        docUuid: editState.docUuid,
        answerSegmentId: editState.answerSegmentId,
        answerContent: editState.answerContent,
        content: editState.content,
      })
    } else {
      await api.documentSegmentChildSaveOrUpdate({
        id: editState.id,
        docUuid: editState.docUuid,
        parentSegmentId: editState.parentSegmentId,
        content: editState.content,
      })
    }
    ms.success(t('knowledgeBase.segmentSavedAndReindexing'))
    editState.show = false
    loadList(paginationReactive.page)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    submitting.value = false
  }
}

function confirmDelete(type: 'segment' | 'question' | 'child', uuid: string) {
  dialog.warning({
    title: t('common.delete'),
    content: t('common.deleteConfirm'),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: async () => {
      try {
        if (type === 'segment')
          await api.documentSegmentDel(uuid)
        else if (type === 'question')
          await api.documentSegmentQuestionDel(uuid)
        else
          await api.documentSegmentChildDel(uuid)
        ms.success(t('common.deleteSuccess'))
        loadList(paginationReactive.page)
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      }
    },
  })
}

function truncated(text: string, len = 60) {
  return text.length > len ? `${text.substring(0, len)}...` : text
}

// 停用分段会删除其向量与图谱数据；启用会重新生成（图谱抽取消耗模型额度），操作前均需确认
function confirmToggleStatus(row: KnowledgeBase.Segment, isEnabled: boolean) {
  dialog.warning({
    title: isEnabled ? t('knowledgeBase.enable') : t('knowledgeBase.disable'),
    content: isEnabled ? t('knowledgeBase.segmentEnableConfirm') : t('knowledgeBase.segmentDisableConfirm'),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: async () => {
      try {
        await api.documentSegmentToggleStatus({ uuid: row.uuid, isEnabled })
        ms.success(t('common.saveSuccess'))
        loadList(paginationReactive.page)
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      }
    },
  })
}

const createColumns = (): DataTableColumns<KnowledgeBase.Segment> => {
  const cols: DataTableColumns<KnowledgeBase.Segment> = [
    {
      title: '#',
      key: 'position',
      width: 60,
      render: row => row.position + 1,
    },
  ]
  // 问答模式下问题列在前：先问后答与"问答对"阅读顺序一致（导入/生成格式同为 question 在前），
  // 也把 1:N 关系摆成 FAQ 心智模型——几种问法，一个答案
  if (segmentMode.value === 'qa') {
    cols.push({
      // 列头问号图标挂提示：一个答案可关联多个问题（单元格即该答案名下的问题集合）
      title: () => h('span', { class: 'flex items-center gap-1' }, {
        default: () => [
          h('span', t('knowledgeBase.relatedQuestions')),
          h(NTooltip, { trigger: 'hover' }, {
            trigger: () => h(NIcon, { size: 14, style: 'cursor: help; opacity: 0.65;' }, { default: () => h(QuestionCircle16Regular) }),
            default: () => t('knowledgeBase.qaQuestionMultiTip'),
          }),
        ],
      }),
      key: 'questions',
      render: row => h('div', { class: 'flex flex-col gap-1' }, {
        default: () => [
          ...(row.questions || []).map(q => h('div', { class: 'flex items-center gap-2' }, {
            default: () => [
              h('span', { style: 'cursor: pointer;', onClick: () => openEdit('question', q) }, { default: () => truncated(q.content, 40) }),
              h(NButton, { text: true, type: 'error', size: 'tiny', onClick: () => confirmDelete('question', q.uuid) }, { default: () => t('common.delete') }),
            ],
          })),
          // 停用段隐藏新增入口（后端已守卫不向量化，此处仅体验优化）
          ...(row.isEnabled === false ? [] : [h(NButton, { text: true, type: 'primary', size: 'tiny', onClick: () => openAddQuestion(row.id) }, { default: () => `+ ${t('knowledgeBase.qaQuestion')}` })]),
        ],
      }),
    })
  }
  cols.push({
    title: segmentMode.value === 'qa'
      ? t('knowledgeBase.qaAnswer')
      : segmentMode.value === 'parent_child'
        ? t('knowledgeBase.segmentModeParentChild')
        : t('knowledgeBase.docFragment'),
    key: 'content',
    render: row => h('div', {
      style: 'cursor: pointer; white-space: pre-wrap;',
      onClick: () => openEdit('segment', row),
    }, { default: () => truncated(row.content) }),
  })
  if (segmentMode.value === 'parent_child') {
    cols.push({
      title: t('knowledgeBase.childChunks'),
      key: 'children',
      render: row => h('div', { class: 'flex flex-col gap-1' }, {
        default: () => [
          ...(row.children || []).map(c => h('div', { class: 'flex items-center gap-2' }, {
            default: () => [
              h('span', { style: 'cursor: pointer;', onClick: () => openEdit('child', c) }, { default: () => truncated(c.content, 40) }),
              h(NButton, { text: true, type: 'error', size: 'tiny', onClick: () => confirmDelete('child', c.uuid) }, { default: () => t('common.delete') }),
            ],
          })),
          ...(row.isEnabled === false ? [] : [h(NButton, { text: true, type: 'primary', size: 'tiny', onClick: () => openAddChild(row.id) }, { default: () => `+ ${t('knowledgeBase.childChunks')}` })]),
        ],
      }),
    })
  }
  cols.push(
    {
      title: t('knowledgeBase.segmentHitCount'),
      key: 'hitCount',
      width: 90,
    },
    {
      title: t('knowledgeBase.wordCount'),
      key: 'wordCount',
      width: 90,
    },
    {
      title: t('knowledgeBase.status'),
      key: 'isEnabled',
      width: 130,
      render: (row) => {
        const elements: any[] = [h(NSwitch, {
          size: 'small',
          value: row.isEnabled !== false,
          disabled: isRebuilding(row),
          onUpdateValue: (value: boolean) => confirmToggleStatus(row, value),
        })]
        if (isRebuilding(row))
          elements.push(h('span', { style: 'font-size:12px;color:#f0a020;margin-left:6px;' }, { default: () => t('knowledgeBase.statusProcessing') }))
        else if (isRebuildFailed(row))
          elements.push(h('span', { style: 'font-size:12px;color:#d03050;margin-left:6px;cursor:pointer;', title: row.failReason || '', onClick: () => retryRebuild(row) }, { default: () => t('knowledgeBase.statusFailed') }))
        return h('div', { class: 'flex items-center' }, { default: () => elements })
      },
    },
    {
      title: t('common.action'),
      key: 'actions',
      width: 80,
      render: row => h(NButton, { text: true, type: 'error', size: 'small', onClick: () => confirmDelete('segment', row.uuid) }, { default: () => t('common.delete') }),
    },
  )
  return cols
}

const columns = computed<DataTableColumns<KnowledgeBase.Segment>>(() => createColumns())

async function onHandlePageChange(currentPage: number) {
  loadList(currentPage)
}

watch(
  () => route.params.docUuid,
  (docUuid) => {
    const uuid = Array.isArray(docUuid) ? docUuid[0] : docUuid
    if (uuid) {
      curDocUuid.value = uuid
      loadDocInfo(uuid)
      loadList(1)
    }
  },
  { immediate: true },
)

onMounted(() => {
  tableMaxHeight.value = window.innerHeight - 420
})

onUnmounted(() => {
  if (refreshTimer)
    clearTimeout(refreshTimer)
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
        {{ curDoc.title }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NAlert
      v-if="curDoc.embeddingStatus === 'FAIL' || curDoc.graphicalStatus === 'FAIL'"
      type="error"
      :show-icon="true"
      style="margin-top: 12px"
    >
      <div class="flex items-center justify-between gap-3">
        <span>{{ curDoc.failReason || t('knowledgeBase.statusFailed') }}</span>
        <NButton size="small" type="error" @click="retryDocIndex">
          {{ t('knowledgeBase.retry') }}
        </NButton>
      </div>
    </NAlert>
    <NCard style="margin-top: 12px" :title="curDoc.title" hoverable>
      <NSpin :show="docLoading">
        <div class="flex items-center gap-2">
          <span style="white-space: pre-wrap;">{{ curDoc.brief }}</span>
          <!-- 正文查看与分段模式无关：remark 是文档本身的内容，模式只决定索引方式 -->
          <NButton text type="primary" size="tiny" @click="showRawContent = true">
            {{ t('knowledgeBase.viewRawContent') }}
          </NButton>
        </div>
        <div class="flex flex-wrap gap-x-6 gap-y-1 mt-2" style="font-size: 12px; opacity: 0.7;">
          <span>{{ t('knowledgeBase.vectorize') }}: {{ embeddingStatusLabel }}</span>
          <span>{{ t('knowledgeBase.wordCount') }}: {{ curDoc.wordCount }}</span>
          <span>{{ t('knowledgeBase.embeddingHitCount') }}: {{ curDoc.embeddingHitCount }}</span>
          <span>{{ t('knowledgeBase.createTime') }}: {{ curDoc.createTime }}</span>
          <span>{{ t('knowledgeBase.updateTime') }}: {{ curDoc.updateTime }}</span>
        </div>
      </NSpin>
    </NCard>
    <NCard style="margin-top: 12px" :title="segmentMode === 'qa' ? t('knowledgeBase.qaPairList') : t('knowledgeBase.segmentList')" hoverable>
      <template #header-extra>
        <NSpace v-if="segmentMode === 'qa' && curDoc.embeddingStatus !== 'DOING'" :wrap="false" :size="8">
          <NButton size="small" type="primary" @click="openAddQaPair">
            {{ t('knowledgeBase.addQaPair') }}
          </NButton>
          <NButton size="small" ghost @click="showQaImportModal = true">
            {{ t('knowledgeBase.importQa') }}
          </NButton>
          <NButton size="small" ghost @click="confirmGenerateQa">
            {{ paginationReactive.itemCount > 0 ? t('knowledgeBase.regenerateQa') : t('knowledgeBase.aiGenerateQa') }}
          </NButton>
        </NSpace>
      </template>
      <NDataTable
        remote :loading="loading" :max-height="tableMaxHeight" :columns="columns" :data="segments"
        :pagination="paginationReactive" :single-line="false" :bordered="true" @update:page="onHandlePageChange"
      >
      </NDataTable>
    </NCard>

    <NModal v-model:show="editState.show" style="width: 60%;" preset="card" :title="editTitle()">
      <NSpace vertical>
        {{ editState.type === 'question' ? t('knowledgeBase.qaQuestion') : '' }}
        <NInput
          v-model:value="editState.content"
          type="textarea"
          :autosize="{ minRows: 6, maxRows: 16 }"
          :placeholder="editState.type === 'question' && editState.isNew ? t('knowledgeBase.qaQuestionLinesPlaceholder') : ''"
        />
        <span v-if="editState.type === 'question'" style="font-size: 12px; opacity: 0.65;">
          {{ t('knowledgeBase.qaQuestionMultiTip') }}
        </span>
        <!-- 新增问答对时的答案输入：问题在上（先问后答，与列序一致） -->
        <template v-if="editState.type === 'question' && editState.isNew && !editState.answerSegmentId">
          {{ t('knowledgeBase.qaAnswer') }}
          <NInput
            v-model:value="editState.answerContent"
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 8 }"
          />
        </template>
        <div class="flex justify-end gap-2">
          <NButton @click="editState.show = false">
            {{ t('common.cancel') }}
          </NButton>
          <NButton type="primary" :loading="submitting" @click="saveEdit">
            {{ t('common.confirm') }}
          </NButton>
        </div>
      </NSpace>
    </NModal>

    <NModal v-model:show="showRawContent" style="width: 70%" preset="card" :title="curDoc.title">
      <div style="white-space: pre-wrap; max-height: 65vh; overflow: auto; font-size: 13px;">
        {{ curDoc.remark }}
      </div>
      <template #footer>
        <div class="flex justify-end gap-2">
          <NButton size="small" @click="showRawContent = false">
            {{ t('common.cancel') }}
          </NButton>
          <NButton size="small" type="primary" @click="goEditDocument">
            {{ t('common.edit') }}
          </NButton>
        </div>
      </template>
    </NModal>

    <NModal v-model:show="showQaImportModal" style="width: 60%;" preset="card" :title="t('knowledgeBase.importQa')">
      <NSpace vertical>
        <NP>{{ t('knowledgeBase.importQaTip') }}</NP>
        <NUpload
          :max="1" accept=".xlsx,.xls,.csv" directory-dnd
          :action="`/api/document/importQa/${curDocUuid}`"
          :headers="qaImportHeaders" @finish="onQaImportFinish"
        >
          <NUploadDragger>
            <NText style="font-size: 16px">
              {{ t('knowledgeBase.clickOrDragToUpload') }}
            </NText>
            <NP depth="3" style="margin: 8px 0 0 0">
              XLSX / CSV
            </NP>
          </NUploadDragger>
        </NUpload>
        <NSpace>
          <NButton type="primary" ghost @click="downloadQaTemplate">
            {{ t('knowledgeBase.downloadTemplate') }}
          </NButton>
        </NSpace>
      </NSpace>
    </NModal>
  </div>
</template>
