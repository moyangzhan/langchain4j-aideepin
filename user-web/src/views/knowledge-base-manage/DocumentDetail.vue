<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCollapse, NCollapseItem, NDataTable, NIcon, NInput, NModal, NP, NSpace, NSwitch, NText, NTooltip, NUpload, NUploadDragger, useDialog, useLoadingBar, useMessage } from 'naive-ui'
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
const curDoc = reactive<KnowledgeBase.Document>(knowledgeBaseEmptyItem())
// Entry loads use the global top loading bar; poll refreshes are fully silent
const loadingBar = useLoadingBar()

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

// Edit/add modal state: type picks the save API (qaPair = pair edit; question + isNew + no
// answerSegmentId = add QA pair). Both use dynamic question inputs — one input per question
const editState = reactive<{
  show: boolean
  type: 'segment' | 'question' | 'child' | 'qaPair'
  id?: string
  docUuid: string
  answerSegmentId?: string
  parentSegmentId?: string
  content: string
  questions: string[]
  answerContent?: string
  isNew: boolean
}>({
  show: false,
  type: 'segment',
  docUuid: '',
  content: '',
  questions: [],
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

// Retry label mirrors what retryIndex actually retries (all failed dimensions, possibly both);
// QA mode with no pairs triggers AI generation instead
const retryDocLabel = computed(() => {
  if (segmentMode.value === 'qa' && paginationReactive.itemCount === 0)
    return t('knowledgeBase.aiGenerateQa')
  const embeddingFailed = curDoc.embeddingStatus === 'FAIL'
  const graphFailed = curDoc.graphicalStatus === 'FAIL'
  if (embeddingFailed && graphFailed)
    return t('knowledgeBase.retryBothIndex')
  if (graphFailed)
    return t('knowledgeBase.retryGraphitize')
  return t('knowledgeBase.retryVectorize')
})

// Failure list: the doc row keeps one fail_reason only; per-dimension reasons come from the task table
const indexFailures = ref<KnowledgeBase.IndexFailure[]>([])

const failureRows = computed(() => {
  const rows: { label: string; reason: string }[] = []
  const findByType = (type: string) => indexFailures.value.find(f => f.taskType === type)
  if (curDoc.embeddingStatus === 'FAIL')
    rows.push({ label: t('knowledgeBase.vectorize'), reason: findByType('embedding')?.failReason || curDoc.failReason || t('knowledgeBase.statusFailed') })
  if (curDoc.graphicalStatus === 'FAIL')
    rows.push({ label: t('knowledgeBase.graphLabel'), reason: findByType('graphical')?.failReason || curDoc.failReason || t('knowledgeBase.statusFailed') })
  return rows
})

const failureSummary = computed(() => {
  const dims: string[] = []
  if (curDoc.embeddingStatus === 'FAIL')
    dims.push(t('knowledgeBase.vectorize'))
  if (curDoc.graphicalStatus === 'FAIL')
    dims.push(t('knowledgeBase.graphLabel'))
  return t('knowledgeBase.indexFailedSummary', { dims: dims.join(', ') })
})

// Indexing in progress = executing (DOING) or queued (unfinished task rows): retry enqueues
// without setting DOING, so the doc stays FAIL while queued and taskUnfinished tells queued
// apart from finally failed
const taskUnfinished = ref(false)
const indexRunning = computed(() => curDoc.embeddingStatus === 'DOING' || curDoc.graphicalStatus === 'DOING')
// Retry submit lock: keep the "processing" state (button hidden) for at least 5s — it covers
// the request round-trip window before taskUnfinished is confirmed and rapid re-clicks on error
const retrySubmitting = ref(false)
const indexInProgress = computed(() => indexRunning.value || taskUnfinished.value || retrySubmitting.value)

const runningSummary = computed(() => {
  const dims: string[] = []
  if (curDoc.embeddingStatus === 'DOING' || (curDoc.embeddingStatus === 'FAIL' && (taskUnfinished.value || retrySubmitting.value)))
    dims.push(t('knowledgeBase.vectorize'))
  if (curDoc.graphicalStatus === 'DOING' || (curDoc.graphicalStatus === 'FAIL' && (taskUnfinished.value || retrySubmitting.value)))
    dims.push(t('knowledgeBase.graphLabel'))
  return t('knowledgeBase.indexRunningSummary', { dims: dims.join(', ') })
})

// While queued or executing, refresh every 3s (doc status + queue status) until it ends:
// done collapses the section, a final failure restores the retry button; a page reload
// mid-queue also recovers the processing state from the task status
let docRefreshTimer: ReturnType<typeof setTimeout> | null = null
function scheduleDocRefresh() {
  if (docRefreshTimer)
    clearTimeout(docRefreshTimer)
  if (indexInProgress.value)
    docRefreshTimer = setTimeout(() => loadDocInfo(curDocUuid.value, true), 3000)
}

function editTitle() {
  if (editState.type === 'qaPair')
    return t('knowledgeBase.editQaPair')
  const prefix = editState.isNew
    ? (editState.type === 'question' ? t('knowledgeBase.addQaPair') : t('knowledgeBase.childChunks'))
    : t('common.edit')
  return prefix
}

// silent=true is a poll refresh: skip re-fetching KB info, swallow errors (next tick retries);
// entry-load progress is the caller's top loading bar, and post-retry refresh has no overlay
async function loadDocInfo(docUuid: string, silent = false) {
  try {
    const docResp = await api.knowledgeBaseItemInfo<KnowledgeBase.Document>(docUuid)
    Object.assign(curDoc, docResp.data)
    if (!silent) {
      const kbResp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid)
      Object.assign(curKb, kbResp.data)
    }
    indexFailures.value = []
    if (curDoc.embeddingStatus === 'FAIL' || curDoc.graphicalStatus === 'FAIL') {
      // Queued vs finally failed is decided by the queue status; a failed fetch degrades to
      // "finally failed" (retryable) without blocking the page
      try {
        const [failures, progress] = await Promise.all([
          api.documentIndexFailures<KnowledgeBase.IndexFailure[]>(docUuid),
          api.documentIndexProgress<boolean>(docUuid),
        ])
        indexFailures.value = failures.data || []
        taskUnfinished.value = !!progress.data
      } catch {
        indexFailures.value = []
        taskUnfinished.value = false
      }
    } else {
      taskUnfinished.value = false
    }
    // 列表为空时（如新建的 QA 文档）用文档自身的分段模式初始化，保证空态下也能新增
    if (segments.value.length === 0 && curDoc.segmentMode)
      segmentMode.value = curDoc.segmentMode as 'text' | 'qa' | 'parent_child'
  } catch (error: any) {
    if (!silent)
      ms.error(error.message ?? 'error')
  } finally {
    scheduleDocRefresh()
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

// Repair vector drift: server clears missing embedding ids and enqueues re-embedding
function confirmRepairVector(row: KnowledgeBase.Segment) {
  dialog.warning({
    title: t('knowledgeBase.vectorMissing'),
    content: t('knowledgeBase.vectorMissingConfirm'),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: async () => {
      try {
        await api.documentSegmentRepairVector(row.uuid)
        ms.success(t('knowledgeBase.segmentSavedAndReindexing'))
        loadList(paginationReactive.page)
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      }
    },
  })
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
    } else {
      ms.error(resp.message || 'error')
    }
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

// Document-level retry: gated on the status columns; guarded against re-trigger while running
async function retryDocIndex() {
  if (indexInProgress.value)
    return
  retrySubmitting.value = true
  // Hold for at least 5s: covers the round-trip before taskUnfinished is confirmed and rapid
  // re-clicks on error; indexInProgress takes over seamlessly when the lock expires
  setTimeout(() => {
    retrySubmitting.value = false
  }, 5000)
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

function goGraph() {
  router.push({ name: 'DocumentGraph', params: { kbUuid, docUuid: curDocUuid.value } })
}

// silent=true is a rebuild poll: no table loading flash; user actions (paging) use non-silent
async function loadList(currentPage: number, silent = false) {
  if (!silent)
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
      await loadList(1, silent)
      return
    }
  } catch (error: any) {
    if (!silent)
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
    refreshTimer = setTimeout(() => loadList(paginationReactive.page, true), 3000)
}

function openEdit(type: 'segment' | 'child', row: any) {
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
    questions: [''],
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

// Pair edit: one input per question (prefilled), answer in its own box; saved via content diff
function openEditQaPair(row: KnowledgeBase.Segment) {
  Object.assign(editState, {
    show: true,
    type: 'qaPair',
    id: row.id,
    docUuid: curDocUuid.value,
    answerSegmentId: row.id,
    content: '',
    questions: (row.questions || []).map(q => q.content),
    answerContent: row.content,
    isNew: false,
  })
}

async function saveEdit() {
  // Question-set types (pair edit / add QA pair): one input per question; trim, drop blanks, dedupe
  const questionListType = editState.type === 'qaPair' || (editState.type === 'question' && editState.isNew && !editState.answerSegmentId)
  const questions = [...new Set(editState.questions.map(q => q.trim()).filter(q => q.length > 0))]
  if (questionListType) {
    if (questions.length === 0 || !(editState.answerContent || '').trim()) {
      ms.warning(t('common.inputPlaceholder'))
      return
    }
  } else if (!editState.content.trim()) {
    ms.warning(t('common.inputPlaceholder'))
    return
  }
  try {
    submitting.value = true
    if (editState.type === 'segment') {
      await api.documentSegmentSaveOrUpdate({ id: editState.id!, docUuid: editState.docUuid, content: editState.content } as KnowledgeBase.Segment)
    } else if (editState.type === 'qaPair') {
      // Server diffs by content: unchanged questions keep vectors, answer-only changes do not
      // re-embed — hence the generic save-success message
      await api.documentQaPairSaveOrUpdate({
        docUuid: editState.docUuid,
        answerSegmentId: editState.answerSegmentId,
        answerContent: editState.answerContent,
        questions,
      })
      ms.success(t('common.saveSuccess'))
      editState.show = false
      loadList(paginationReactive.page)
      return
    } else if (editState.type === 'question') {
      // Add QA pair: the first question creates the answer segment; the rest attach to it
      const first = await api.documentSegmentQuestionSaveOrUpdate<any>({
        docUuid: editState.docUuid,
        answerContent: editState.answerContent,
        content: questions[0],
      })
      const answerSegmentId: string | undefined = first.data?.answerSegmentId
      for (const question of questions.slice(1)) {
        await api.documentSegmentQuestionSaveOrUpdate<any>({
          docUuid: editState.docUuid,
          answerSegmentId,
          content: question,
        })
      }
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

function confirmDelete(type: 'segment' | 'child', uuid: string) {
  dialog.warning({
    title: t('common.delete'),
    content: t('common.deleteConfirm'),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: async () => {
      try {
        if (type === 'segment')
          await api.documentSegmentDel(uuid)
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
      // Questions column is display-only: per-question edits go through the action column's edit
      render: row => h('div', { class: 'flex flex-col gap-1' }, {
        default: () => (row.questions || []).map(q => h('div', { class: 'truncate' }, { default: () => truncated(q.content, 40) })),
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
    // qa mode: answers are edited via the action column; other modes keep click-to-edit
    render: row => h('div', {
      style: `white-space: pre-wrap;${segmentMode.value === 'qa' ? '' : 'cursor: pointer;'}`,
      onClick: segmentMode.value === 'qa' ? undefined : () => openEdit('segment', row),
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
        // Drift indicator: status says vectorized but the store lacks the vector
        if (row.vectorMissing && row.isEnabled !== false && !isRebuilding(row))
          elements.push(h('span', { style: 'font-size:12px;color:#d03050;margin-left:6px;cursor:pointer;', onClick: () => confirmRepairVector(row) }, { default: () => t('knowledgeBase.vectorMissing') }))
        return h('div', { class: 'flex items-center' }, { default: () => elements })
      },
    },
    {
      title: t('common.action'),
      key: 'actions',
      width: segmentMode.value === 'qa' ? 100 : 80,
      // qa mode: pair-edit entry; other modes keep delete only (content edits via content column)
      render: row => segmentMode.value === 'qa'
        ? h('div', { class: 'flex items-center gap-2' }, {
          default: () => [
            h(NButton, { text: true, type: 'primary', size: 'small', onClick: () => openEditQaPair(row) }, { default: () => t('common.edit') }),
            h(NButton, { text: true, type: 'error', size: 'small', onClick: () => confirmDelete('segment', row.uuid) }, { default: () => t('common.delete') }),
          ],
        })
        : h(NButton, { text: true, type: 'error', size: 'small', onClick: () => confirmDelete('segment', row.uuid) }, { default: () => t('common.delete') }),
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
      // Entry loads share the global top loading bar; no local spinners
      loadingBar.start()
      Promise.all([loadDocInfo(uuid), loadList(1)]).finally(() => loadingBar.finish())
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
  if (docRefreshTimer)
    clearTimeout(docRefreshTimer)
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
    <NCard style="margin-top: 12px" :title="curDoc.title" hoverable>
      <div style="white-space: pre-wrap;">
        {{ curDoc.brief }}
      </div>
      <div class="flex flex-wrap gap-x-6 gap-y-1 mt-2" style="font-size: 12px; opacity: 0.7;">
        <span>{{ t('knowledgeBase.vectorize') }}: {{ embeddingStatusLabel }}</span>
        <span>{{ t('knowledgeBase.wordCount') }}: {{ curDoc.wordCount }}</span>
        <span>{{ t('knowledgeBase.embeddingHitCount') }}: {{ curDoc.embeddingHitCount }}</span>
        <span>{{ t('knowledgeBase.createTime') }}: {{ curDoc.createTime }}</span>
        <span>{{ t('knowledgeBase.updateTime') }}: {{ curDoc.updateTime }}</span>
      </div>
      <!-- content → metadata → actions -->
      <div class="flex items-center gap-4 mt-3">
        <!-- remark is the document's own content regardless of segment mode -->
        <NButton text type="primary" size="tiny" @click="showRawContent = true">
          {{ t('knowledgeBase.viewRawContent') }}
        </NButton>
        <!-- Same rule as the list page: disabled with a tooltip when not graphitized;
               native disabled buttons swallow mouse events, so the tooltip wraps it -->
        <NTooltip v-if="curDoc.graphicalStatus === 'NONE'" trigger="hover">
          <template #trigger>
            <span class="inline-flex">
              <NButton text type="primary" size="tiny" disabled>
                {{ t('knowledgeBase.openGraph') }}
              </NButton>
            </span>
          </template>
          {{ t('knowledgeBase.notGraphitized') }}
        </NTooltip>
        <NButton v-else text type="primary" size="tiny" @click="goGraph">
          {{ t('knowledgeBase.openGraph') }}
        </NButton>
      </div>
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
      />
    </NCard>

    <!-- Failure list: collapsed by default, the header keeps failures visible; expanding shows
         per-dimension reasons. While retrying it switches to the running state (button hidden)
         and auto-refreshes via polling -->
    <NCollapse v-if="failureRows.length > 0 || indexInProgress" style="margin-top: 12px">
      <NCollapseItem name="indexFailures">
        <template #header>
          <div class="flex items-center justify-between w-full pr-2">
            <span :style="{ color: indexInProgress ? '#f0a020' : '#d03050', fontWeight: '600' }">
              {{ indexInProgress ? runningSummary : failureSummary }}
            </span>
            <NButton v-if="!indexInProgress" size="small" type="error" @click.stop="retryDocIndex">
              {{ retryDocLabel }}
            </NButton>
          </div>
        </template>
        <ul v-if="failureRows.length > 0" class="flex flex-col gap-1 m-0 p-0" style="list-style: none;">
          <li v-for="row in failureRows" :key="row.label">
            <span style="font-weight: 600;">{{ row.label }}:</span> {{ row.reason }}
          </li>
        </ul>
      </NCollapseItem>
    </NCollapse>

    <NModal v-model:show="editState.show" style="width: 60%;" preset="card" :title="editTitle()">
      <NSpace vertical>
        <!-- Question set (add QA pair / pair edit): one input per question; the API layer
             collapses any newlines so users never need to care -->
        <template v-if="editState.type === 'qaPair' || (editState.type === 'question' && editState.isNew && !editState.answerSegmentId)">
          <div>{{ t('knowledgeBase.qaQuestion') }}</div>
          <div v-for="(_, idx) in editState.questions" :key="idx" class="flex items-center gap-2">
            <NInput v-model:value="editState.questions[idx]" :placeholder="t('knowledgeBase.qaQuestionInputPlaceholder')" />
            <NButton
              v-if="editState.questions.length > 1" text type="error" size="tiny"
              @click="editState.questions.splice(idx, 1)"
            >
              ✕
            </NButton>
          </div>
          <NButton dashed size="small" @click="editState.questions.push('')">
            + {{ t('knowledgeBase.addQuestion') }}
          </NButton>
          <span style="font-size: 12px; opacity: 0.65;">{{ t('knowledgeBase.qaQuestionMultiTip') }}</span>
        </template>
        <!-- text segment / qa answer / parent chunk: multiline content -->
        <template v-else>
          <NInput
            v-model:value="editState.content"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 16 }"
          />
        </template>
        <!-- Answer input for add-pair / pair-edit; questions first, matching column order -->
        <template v-if="editState.type === 'qaPair' || (editState.type === 'question' && editState.isNew && !editState.answerSegmentId)">
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
