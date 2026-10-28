<script setup lang='ts'>
import { computed, h, onActivated, onMounted, reactive, ref, watch } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NCheckboxGroup, NDataTable, NFlex, NInput, NModal, NTag, useDialog, useMessage } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'
import { createColumns } from './documentColumns'
import FilePreviewModal from './components/FilePreviewModal.vue'
import { ApiKeyModal } from '@/components/common'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore } from '@/store'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const dialog = useDialog()
const route = useRoute()
const router = useRouter()
const { kbUuid: curKbUuid } = route.params as { kbUuid: string; kbId: string }

const modalMainHeight = ref<number>(500)
const tableMaxHeight = ref<number>(500)
const loading = ref<boolean>(false)
const showIndexModal = ref<boolean>(false)
const itemList = ref<KnowledgeBase.Document[]>([])
const indexTypeSelected = ref<string[]>(['embedding'])

const paginationReactive = reactive({
  page: 1,
  pageSize: 10,
  itemCount: 0,
  prefix: () => t('common.total', { n: paginationReactive.itemCount }),
})
const searchValue = ref<string>('')
const { isMobile } = useBasicLayout()
const authStore = useAuthStore()
const token = ref<string>(authStore.token)
const checkedItemRowKeys = ref<string[]>([])
const checkedItems = ref<KnowledgeBase.Document[]>([])
const curKnowledgeBase: KnowledgeBase.Info = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const showApiKeyModal = ref(false)

// 文件预览
const filePreview = reactive({
  show: false,
  url: '',
  name: '',
})

const showFileContent = (selected: KnowledgeBase.Document = knowledgeBaseEmptyItem()) => {
  filePreview.url = selected.sourceFileUrl || ''
  filePreview.name = selected.sourceFileName || selected.title
  filePreview.show = true
}

const viewSegments = (row: KnowledgeBase.Document) => {
  router.push({ name: 'DocumentDetail', params: { kbUuid: curKbUuid, docUuid: row.uuid } })
}

async function showIndexStatus(row: KnowledgeBase.Document, taskType: 'embedding' | 'graphical') {
  const isEmbedding = taskType === 'embedding'
  const status = isEmbedding ? row.embeddingStatus : row.graphicalStatus
  const changeTime = isEmbedding ? row.embeddingStatusChangeTime : row.graphicalStatusChangeTime
  if (status === 'DONE') {
    dialog.info({
      title: t(isEmbedding ? 'knowledgeBase.vectorize' : 'knowledgeBase.graphLabel'),
      content: t('knowledgeBase.statusFinishTime', { time: changeTime || '-' }),
      positiveText: t('common.confirm'),
    })
    return
  }
  try {
    const resp = await api.documentIndexFailures<KnowledgeBase.IndexFailure[]>(row.uuid)
    const failure = (resp.data || []).find(f => f.taskType === taskType)
    dialog.warning({
      title: t(isEmbedding ? 'knowledgeBase.embeddingFailReasonTitle' : 'knowledgeBase.graphFailReasonTitle'),
      content: failure
        ? () => h('div', { class: 'flex flex-col gap-1' }, [
            h('div', null, `${t('knowledgeBase.failTimeLabel')}：${failure.updateTime}`),
            h('div', null, `${t('knowledgeBase.failContentLabel')}：${failure.userMsg || failure.failReason}`),
          ])
        : t('knowledgeBase.indexFailReasonEmpty'),
      positiveText: t('common.confirm'),
    })
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  }
}

const editItem = (row: KnowledgeBase.Document) => {
  router.push({ name: 'DocumentEdit', params: { kbUuid: curKbUuid, docUuid: row.uuid } })
}

function rowKey(row: KnowledgeBase.Document) {
  return row.uuid
}

// Serial-number column width auto-fits to the digit count of total rows, with a floor
// wide enough for 3 digits (the old 40px floor made two-digit numbers wrap)
const serialColWidth = computed(() => {
  const digits = Math.max(String(Math.max(paginationReactive.itemCount, 1)).length, 3)
  return digits * 10 + 28
})
const columns = computed(() => {
  const cols = createColumns({ showIndexStatus, viewSegments, showFileContent, editItem, deleteKbItem, toggleStatus })
  cols.splice(1, 0, {
    title: '#',
    key: 'serialNumber',
    width: serialColWidth.value,
    align: 'center',
    render: (_row, index) => (paginationReactive.page - 1) * paginationReactive.pageSize + index + 1,
  })
  return cols
})

function changeIndexModal() {
  showIndexModal.value = true
}

// 库级重建：embedding=按现有分段重嵌（问答文档重嵌问题）；graphical=重抽图谱
// KB-level rebuild: embedding re-embeds existing segments (QA docs re-embed questions); graphical re-extracts
function confirmRebuild(indexType: 'embedding' | 'graphical') {
  const d = dialog.warning({
    title: t(indexType === 'embedding' ? 'knowledgeBase.rebuildEmbedding' : 'knowledgeBase.rebuildGraphical'),
    content: indexType === 'embedding'
      ? t('knowledgeBase.rebuildEmbeddingKbConfirm', { title: curKnowledgeBase.title })
      : () => h('span', null, [
          t('knowledgeBase.rebuildGraphicalKbConfirmPrefix', { title: curKnowledgeBase.title, model: curKnowledgeBase.ingestModelName || '-' }),
          h('strong', null, t('knowledgeBase.rebuildGraphicalKbConfirmToken')),
          t('knowledgeBase.rebuildGraphicalKbConfirmSuffix'),
        ]),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: async () => {
      // 返回 Promise 时 naive-ui 会等请求结束才关弹窗，期间按钮可被连点：置 loading 防重复提交
      d.loading = true
      try {
        await api.knowledgeBaseRebuild(curKbUuid, indexType)
        ms.success(t('knowledgeBase.indexTaskRunning'))
        indexingCheck()
        search(1)
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      } finally {
        d.loading = false
      }
    },
  })
}

/**
 * 索引文档
 */
async function textIndexing() {
  if (checkedItemRowKeys.value.length === 0) {
    ms.warning(t('knowledgeBase.selectAtLeastOneRow'))
    return
  }
  if (indexTypeSelected.value.length === 0) {
    ms.warning(t('knowledgeBase.selectAtLeastOneIndexType'))
    return
  }
  if (loading.value) {
    ms.warning('indexing')
    return
  }
  showIndexModal.value = false
  loading.value = true
  try {
    await api.knowledgeBaseItemsIndexing(checkedItemRowKeys.value, indexTypeSelected.value)
    indexingCheck()
    ms.success(t('knowledgeBase.indexTaskRunning'))
    search(1)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    loading.value = false
  }
}

/**
 * 检查索引是否已经完成，如果已完成，则刷新列表
 */
async function indexingCheck() {
  const response = await api.knowledgeBaseIndexingCheck()
  if (response.data) {
    search(1)
  } else {
    setTimeout(() => {
      indexingCheck()
    }, 3000)
  }
}

function onHandleCheckedRowKeys(keys: Array<string | number>, rows: object[], _meta: { row: object | undefined; action: 'check' | 'uncheck' | 'checkAll' | 'uncheckAll' }) {
  checkedItemRowKeys.value = keys.map((key) => {
    return `${key}`
  })
  // 跨页面选择时，rows 中的非当前页的数据为 null，所以将 null 过滤掉，并将非当前页的值填充
  const itemMap = new Map<string, KnowledgeBase.Document>()
  const tmpItems = [] as KnowledgeBase.Document[]
  tmpItems.push(...(rows as KnowledgeBase.Document[]))
  tmpItems.push(...checkedItems.value)
  tmpItems.forEach((item) => {
    if (item)
      itemMap.set(item.uuid, item)
  })
  checkedItems.value = Array.from(itemMap.entries())
    .filter(([key]) => checkedItemRowKeys.value.includes(key))
    .map(([, value]) => value)
}

function removeCheckedItem(item: KnowledgeBase.Document) {
  checkedItemRowKeys.value = checkedItemRowKeys.value.filter((key) => {
    return key !== item.uuid
  })
  checkedItems.value = checkedItems.value.filter((row) => {
    return row.uuid !== item.uuid
  })
}

async function onHandlePageChange(currentPage: number) {
  search(currentPage)
}

async function onKeyUpSearch(event: KeyboardEvent) {
  if (event.key === 'Enter' && !event.shiftKey) {
    event.preventDefault()
    search(1)
  }
}

async function search(currentPage: number) {
  loading.value = true
  try {
    const resp = await api.knowledgeBaseItemSearch<PageResponse>(currentPage, paginationReactive.pageSize, curKbUuid, searchValue.value)
    setResp(currentPage, resp.data)
  } finally {
    loading.value = false
  }
}

function setResp(currentPage: number, data: PageResponse) {
  itemList.value = data.records
  paginationReactive.page = currentPage
  paginationReactive.itemCount = data.total
}

function deleteKbItem(row: KnowledgeBase.Document) {
  dialog.warning({
    title: t('knowledgeBase.deleteConfirmTitle'),
    content: t('common.deleteNotRecover'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: async () => {
      try {
        await api.knowledgeBaseItemDelete(row.uuid)
        checkedItemRowKeys.value = checkedItemRowKeys.value.filter(uuid => uuid !== row.uuid)
        itemList.value = itemList.value.filter(item => item.uuid !== row.uuid)
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      }
    },
  })
}

function confirmBatchDelete() {
  if (checkedItemRowKeys.value.length === 0)
    return
  const d = dialog.warning({
    title: t('knowledgeBase.deleteConfirmTitle'),
    content: t('knowledgeBase.batchDeleteConfirm', { count: checkedItemRowKeys.value.length }),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: async () => {
      const selected = [...checkedItemRowKeys.value]
      // 同 confirmRebuild：置 loading 防连点重复提交
      d.loading = true
      try {
        await api.knowledgeBaseItemDeleteBatch(selected)
        checkedItemRowKeys.value = []
        ms.success(t('common.deleteSuccess'))
      } catch (error: any) {
        ms.error(error.message ?? 'error')
      } finally {
        d.loading = false
        search(paginationReactive.page)
      }
    },
  })
}

function toggleStatus(row: KnowledgeBase.Document, isEnabled: boolean) {
  const action = isEnabled ? t('knowledgeBase.enable') : t('knowledgeBase.disable')
  dialog.warning({
    title: t('knowledgeBase.deleteConfirmTitle'),
    content: t('knowledgeBase.toggleStatusConfirm', { action }),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: async () => {
      await api.knowledgeBaseItemToggleStatus(row.uuid, isEnabled)
      row.isEnabled = isEnabled
    },
  })
}

async function initData() {
  search(1)
  const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(curKbUuid)
  Object.assign(curKnowledgeBase, resp.data)
}

const inited = ref<boolean>(false)
onMounted(async () => {
  modalMainHeight.value = window.innerHeight - 150
  tableMaxHeight.value = window.innerHeight - 420
  if (curKnowledgeBase.title === '')
    await initData()
  inited.value = true
})
// Refresh the current page on return from the edit/detail pages; returning from the KB edit
// page also reloads the KB info (title/description/visibility may have changed)
const cameFromRoute = ref('')
router.afterEach((to, from) => {
  if (to.name === 'KnowledgeBaseManageDetail')
    cameFromRoute.value = (from.name as string) || ''
})
onActivated(() => {
  if (!inited.value)
    return
  if (cameFromRoute.value === 'KnowledgeBaseEdit') {
    initData()
    search(paginationReactive.page)
    return
  }
  if (['DocumentAdd', 'DocumentEdit', 'DocumentDetail', 'DocumentUpload'].includes(cameFromRoute.value)) {
    search(paginationReactive.page)
    // uploaded files index asynchronously: poll until the status column settles
    if (cameFromRoute.value === 'DocumentUpload')
      indexingCheck()
  }
})
watch(
  () => token,
  () => {
    if (token.value)
      initData()
  },
  { immediate: true },
)
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
      <NBreadcrumbItem :clickable="false">
        {{ curKnowledgeBase.title }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NCard
      style="margin-top: 12px"
      :title="`${t('knowledgeBase.knowledgeBase')}: ${curKnowledgeBase.title}(${curKnowledgeBase.isPublic ? t('common.public') : t('common.private')})`" hoverable
    >
      <template #header-extra>
        <NFlex align="center" :size="8">
          <NButton tertiary size="small" type="info" @click="showApiKeyModal = true">
            {{ t('extApi.apiAccess') }}
          </NButton>
          <NButton tertiary size="small" type="info" @click="router.push({ name: 'KnowledgeBaseEdit', params: { kbUuid: curKbUuid } })">
            {{ t('common.edit') }}
          </NButton>
          <NButton tertiary size="small" type="warning" @click="confirmRebuild('embedding')">
            {{ t('knowledgeBase.rebuildEmbedding') }}
          </NButton>
          <NButton tertiary size="small" type="warning" @click="confirmRebuild('graphical')">
            {{ t('knowledgeBase.rebuildGraphical') }}
          </NButton>
        </NFlex>
      </template>
      {{ curKnowledgeBase.remark }}
    </NCard>
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.generatedKnowledge')" hoverable>
      <div class="flex gap-3 mb-4" :class="[isMobile ? 'flex-col' : 'flex-row justify-between']">
        <div class="flex items-left gap-2">
          <NButton type="primary" size="small" @click="router.push({ name: 'DocumentAdd', params: { kbUuid: curKbUuid } })">
            {{ t('knowledgeBase.addByForm') }}
          </NButton>
          <NButton type="primary" size="small" @click="router.push({ name: 'DocumentUpload', params: { kbUuid: curKbUuid } })">
            {{ t('knowledgeBase.addByFile') }}
          </NButton>
          <NButton type="primary" size="small" @click="changeIndexModal()">
            {{ t('knowledgeBase.indexSelected') }}
            <template v-if="checkedItemRowKeys.length > 0">
              ({{ checkedItemRowKeys.length }}{{ t('knowledgeBase.item') }})
            </template>
          </NButton>
          <NButton type="error" size="small" ghost :disabled="checkedItemRowKeys.length === 0" @click="confirmBatchDelete">
            {{ t('common.delete') }}
            <template v-if="checkedItemRowKeys.length > 0">
              ({{ checkedItemRowKeys.length }}{{ t('knowledgeBase.item') }})
            </template>
          </NButton>
        </div>
        <div class="flex items-center">
          <NInput v-model:value="searchValue" style="width: 100%" @keyup="onKeyUpSearch" />
          <NButton type="primary" ghost @click="search(1)">
            {{ t('common.search') }}
          </NButton>
        </div>
      </div>
      <NDataTable
        remote :loading="loading" :max-height="tableMaxHeight" :columns="columns" :data="itemList" :pagination="paginationReactive"
        :single-line="false" :bordered="true" :row-key="rowKey" :checked-row-keys="checkedItemRowKeys"
        @update:checked-row-keys="onHandleCheckedRowKeys" @update:page="onHandlePageChange"
      />
    </NCard>
  </div>

  <NModal v-model:show="showIndexModal" style="width: 90%; max-width:550px" preset="card" :title="t('knowledgeBase.selectIndexType')">
    <NFlex vertical>
      <NAlert :title="t('common.tip')" type="info">
        {{ t('knowledgeBase.graphExplanation') }}
      </NAlert>
      <NCheckboxGroup v-model:value="indexTypeSelected" class="my-2">
        <NFlex vertical>
          <NCheckbox value="embedding" :label="t('knowledgeBase.vectorize')" />
          <NCheckbox value="graphical" :label="t('knowledgeBase.graphitize')" />
        </NFlex>
      </NCheckboxGroup>
      <div class="flex flex-wrap space-x-2">
        <NTag
          v-for="checkedItem in checkedItems" :key="`_${checkedItem.uuid}`" :bordered="false" type="info" closable
          size="small" class="mt-1" @close="removeCheckedItem(checkedItem)"
        >
          {{ checkedItem.title }}
        </NTag>
        <NTag v-if="checkedItems.length === 0" :bordered="false" type="warning" size="small">
          {{ t('knowledgeBase.selectKnowledgeFirst') }}
        </NTag>
      </div>
      <NButton
        type="primary" size="small" :disabled="checkedItems.length === 0 || indexTypeSelected.length === 0"
        @click="textIndexing()"
      >
        {{ t('common.confirm') }}
      </NButton>
    </NFlex>
  </NModal>
  <FilePreviewModal v-model:show="filePreview.show" :file-url="filePreview.url" :file-name="filePreview.name" />
  <ApiKeyModal v-model:show="showApiKeyModal" type="knowledge" :uuid="curKbUuid" :title="curKnowledgeBase.title" />
</template>
