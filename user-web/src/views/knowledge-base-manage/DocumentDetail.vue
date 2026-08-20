<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCard, NDataTable, NInput, NModal, NSpace, NSpin, useDialog, useMessage } from 'naive-ui'
import { computed, h, onMounted, reactive, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const route = useRoute()
const ms = useMessage()
const dialog = useDialog()

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
  }
  catch (error: any) {
    ms.error(error.message ?? 'error')
  }
  finally {
    docLoading.value = false
  }
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
  }
  catch (error: any) {
    ms.error(error.message ?? 'error')
  }
  finally {
    loading.value = false
  }
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
    }
    else if (editState.type === 'question') {
      await api.documentSegmentQuestionSaveOrUpdate({
        id: editState.id,
        docUuid: editState.docUuid,
        answerSegmentId: editState.answerSegmentId,
        answerContent: editState.answerContent,
        content: editState.content,
      })
    }
    else {
      await api.documentSegmentChildSaveOrUpdate({
        id: editState.id,
        docUuid: editState.docUuid,
        parentSegmentId: editState.parentSegmentId,
        content: editState.content,
      })
    }
    ms.success(t('common.saveSuccess'))
    editState.show = false
    loadList(paginationReactive.page)
  }
  catch (error: any) {
    ms.error(error.message ?? 'error')
  }
  finally {
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
      }
      catch (error: any) {
        ms.error(error.message ?? 'error')
      }
    },
  })
}

function truncated(text: string, len = 60) {
  return text.length > len ? `${text.substring(0, len)}...` : text
}

const createColumns = (): DataTableColumns<KnowledgeBase.Segment> => {
  const cols: DataTableColumns<KnowledgeBase.Segment> = [
    {
      title: '#',
      key: 'position',
      width: 60,
      render: row => row.position + 1,
    },
    {
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
    },
  ]
  if (segmentMode.value === 'qa') {
    cols.push({
      title: t('knowledgeBase.qaQuestion'),
      key: 'questions',
      render: row => h('div', { class: 'flex flex-col gap-1' }, {
        default: () => [
          ...(row.questions || []).map(q => h('div', { class: 'flex items-center gap-2' }, {
            default: () => [
              h('span', { style: 'cursor: pointer;', onClick: () => openEdit('question', q) }, { default: () => truncated(q.content, 40) }),
              h(NButton, { text: true, type: 'error', size: 'tiny', onClick: () => confirmDelete('question', q.uuid) }, { default: () => t('common.delete') }),
            ],
          })),
          h(NButton, { text: true, type: 'primary', size: 'tiny', onClick: () => openAddQuestion(row.id) }, { default: () => `+ ${t('knowledgeBase.qaQuestion')}` }),
        ],
      }),
    })
  }
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
          h(NButton, { text: true, type: 'primary', size: 'tiny', onClick: () => openAddChild(row.id) }, { default: () => `+ ${t('knowledgeBase.childChunks')}` }),
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
      <NSpin :show="docLoading">
        {{ curDoc.brief }}
        <div class="flex flex-wrap gap-x-6 gap-y-1 mt-2" style="font-size: 12px; opacity: 0.7;">
          <span>{{ t('knowledgeBase.vectorize') }}: {{ embeddingStatusLabel }}</span>
          <span>{{ t('knowledgeBase.wordCount') }}: {{ curDoc.wordCount }}</span>
          <span>{{ t('knowledgeBase.embeddingHitCount') }}: {{ curDoc.embeddingHitCount }}</span>
          <span>{{ t('knowledgeBase.createTime') }}: {{ curDoc.createTime }}</span>
          <span>{{ t('knowledgeBase.updateTime') }}: {{ curDoc.updateTime }}</span>
        </div>
      </NSpin>
    </NCard>
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.segmentList')" hoverable>
      <NDataTable
        remote :loading="loading" :max-height="tableMaxHeight" :columns="columns" :data="segments"
        :pagination="paginationReactive" :single-line="false" :bordered="true" @update:page="onHandlePageChange"
      >
        <template #empty>
          <NSpace vertical align="center">
            <NButton v-if="segmentMode === 'qa'" type="primary" size="small" @click="openAddQaPair">
              + {{ t('knowledgeBase.qaQuestion') }}
            </NButton>
          </NSpace>
        </template>
      </NDataTable>
    </NCard>

    <NModal v-model:show="editState.show" style="width: 60%;" preset="card" :title="editTitle()">
      <NSpace vertical>
        <template v-if="editState.type === 'question' && editState.isNew && !editState.answerSegmentId">
          {{ t('knowledgeBase.qaAnswer') }}
          <NInput
            v-model:value="editState.answerContent"
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 8 }"
          />
        </template>
        {{ editState.type === 'question' ? t('knowledgeBase.qaQuestion') : '' }}
        <NInput
          v-model:value="editState.content"
          type="textarea"
          :autosize="{ minRows: 6, maxRows: 16 }"
        />
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
  </div>
</template>
