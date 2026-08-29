<script setup lang='ts'>
import { computed, nextTick, onActivated, onMounted, reactive, ref, watch } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NCheckboxGroup, NDataTable, NFlex, NIcon, NInput, NInputNumber, NModal, NP, NSelect, NSpace, NTag, NText, NUpload, NUploadDragger, useDialog, useMessage } from 'naive-ui'
import { ArchiveOutline } from '@vicons/ionicons5'
import { useRoute, useRouter } from 'vue-router'
import type { UploadFileInfo, UploadInst } from 'naive-ui'
import { createColumns } from './documentColumns'
import FilePreviewModal from './components/FilePreviewModal.vue'
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
console.log('knowledge-base uuid', curKbUuid)

const modalMainHeight = ref<number>(500)
const tableMaxHeight = ref<number>(500)
const loading = ref<boolean>(false)
const showUploadModal = ref<boolean>(false)
const showIndexModal = ref<boolean>(false)
const itemList = ref<KnowledgeBase.Document[]>([])
const indexTypeSelected = ref<string[]>(['embedding'])
const uploadRef = ref<UploadInst | null>(null)
const headers = { Authorization: '' }
const fileList = ref<UploadFileInfo[]>([])

// Upload-modal segment mode: in qa mode files are parsed as Q&A pair data (Dify format),
// one Q&A document per file, vectorized immediately
const uploadSegmentMode = ref<string>('text')
const uploadChildMaxChunkSize = ref<number | null>(200)
const segmentModeOptions = [
  { label: t('knowledgeBase.segmentModeText'), value: 'text' },
  { label: t('knowledgeBase.segmentModeQa'), value: 'qa' },
  { label: t('knowledgeBase.segmentModeParentChild'), value: 'parent_child' },
]
const uploadAccept = computed(() => (uploadSegmentMode.value === 'qa' ? '.xlsx,.xls,.csv' : undefined))
const uploadAction = computed(() => {
  let url = `/api/knowledge-base/upload/${curKbUuid}?segmentMode=${uploadSegmentMode.value}`
  if (uploadSegmentMode.value === 'parent_child' && uploadChildMaxChunkSize.value)
    url += `&childMaxChunkSize=${uploadChildMaxChunkSize.value}`
  return url
})
const uploadModeTip = computed(() => ({
  text: t('knowledgeBase.segmentModeTextTip'),
  qa: t('knowledgeBase.segmentModeQaTip'),
  parent_child: t('knowledgeBase.segmentModeParentChildTip'),
}[uploadSegmentMode.value] ?? ''))
// Switching the mode clears the picked files
watch(uploadSegmentMode, () => {
  fileList.value = []
})
const fileListLength = computed(() => fileList.value.length)
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

const viewSegments = (row: KnowledgeBase.Document) => {
  router.push({ name: 'DocumentDetail', params: { kbUuid: curKbUuid, docUuid: row.uuid } })
}

const showGraph = (row: KnowledgeBase.Document) => {
  router.push({ name: 'DocumentGraph', params: { kbUuid: curKbUuid, docUuid: row.uuid } })
}

const editItem = (row: KnowledgeBase.Document) => {
  router.push({ name: 'DocumentEdit', params: { kbUuid: curKbUuid, docUuid: row.uuid } })
}

function rowKey(row: KnowledgeBase.Document) {
  return row.uuid
}

// 序号列宽度按总条数位数自适应，恰好容纳最大序号
// Serial-number column width auto-fits to the digit count of total rows
const serialColWidth = computed(() => {
  const digits = String(Math.max(paginationReactive.itemCount, 1)).length
  return Math.max(40, digits * 8 + 24)
})
const columns = computed(() => {
  const cols = createColumns({ viewSegments, showGraph, showFileContent, editItem, deleteKbItem, toggleStatus })
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

function onHandleCheckedRowKeys(keys: Array<string | number>, rows: object[], meta: { row: object | undefined; action: 'check' | 'uncheck' | 'checkAll' | 'uncheckAll' }) {
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

async function onUploadBefore(data: {
  file: UploadFileInfo
  fileList: UploadFileInfo[]
}) {
  // qa mode: accept only filters the file picker, not drag-and-drop; guard the extension here
  if (uploadSegmentMode.value === 'qa' && !/\.(xlsx|xls|csv)$/i.test(data.file.name)) {
    ms.error(t('knowledgeBase.qaUploadFileHint'))
    return false
  }
  return true
}

// stays true when any file in the current submit failed; the modal is kept open so
// the failed rows and the error message stay visible
let uploadHasError = false

function markUploadFailed(file: UploadFileInfo) {
  const row = fileList.value.find(f => f.id === file.id)
  if (row)
    row.status = 'error'
}

function onUploadError({ file }: { file: UploadFileInfo; event?: ProgressEvent }) {
  uploadHasError = true
  markUploadFailed(file)
  ms.error(t('common.uploadFailed'))
}

// true from submit until every file settles (uploaded / failed) or the modal is closed:
// drives the button's loading state and blocks double submits
const uploadSubmitting = ref(false)

function onUploadSubmit() {
  if (uploadSubmitting.value)
    return
  uploadHasError = false
  uploadSubmitting.value = true
  uploadRef.value?.submit()
  closeWhenUploadDone()
}

function closeWhenUploadDone() {
  setTimeout(() => {
    if (!showUploadModal.value) {
      uploadSubmitting.value = false
      return
    }
    const busy = fileList.value.some(f => f.status === 'pending' || f.status === 'uploading')
    if (busy) {
      closeWhenUploadDone()
      return
    }
    uploadSubmitting.value = false
    if (!uploadHasError) {
      showUploadModal.value = false
      search(1)
    }
    indexingCheck()
  }, 1000)
}

function onUploadFinish({
  file,
  event,
}: {
  file: UploadFileInfo
  event?: ProgressEvent
}) {
  const respData = JSON.parse((event?.target as XMLHttpRequest).response)
  if (!respData) {
    uploadHasError = true
    markUploadFailed(file)
    ms.error(t('knowledgeBase.uploadFailedResponseError'))
    return file
  }
  const { success, message } = respData
  console.log('onUploadFinish', success, message)
  if (success) {
    ms.success(t('common.uploadSuccess'))
    indexingCheck()
  } else {
    uploadHasError = true
    markUploadFailed(file)
    ms.error(message || t('common.uploadFailed'))
  }

  return file
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
    onPositiveClick: () => {
      api.knowledgeBaseItemDelete(row.uuid)
      nextTick(() => {
        itemList.value = itemList.value.filter(item => item.uuid !== row.uuid)
      })
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
  if (['DocumentAdd', 'DocumentEdit', 'DocumentDetail'].includes(cameFromRoute.value))
    search(paginationReactive.page)
})
watch(
  () => token,
  () => {
    if (token.value) {
      initData()
      headers.Authorization = token.value
    }
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
          <NButton tertiary size="small" type="info" @click="router.push({ name: 'KnowledgeBaseEdit', params: { kbUuid: curKbUuid } })">
            {{ t('common.edit') }}
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
          <NButton type="primary" size="small" @click="() => showUploadModal = !showUploadModal">
            {{ t('knowledgeBase.addByFile') }}
          </NButton>
          <NButton type="primary" size="small" @click="changeIndexModal()">
            {{ t('knowledgeBase.indexSelected') }}
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

  <!-- Upload files -->
  <NModal v-model:show="showUploadModal" style="width: 90%;  min-height: 700px;" preset="card" :title="t('knowledgeBase.knowledgeItemUpload')">
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.uploadDocToGenerate')" hoverable>
      <NSpace vertical>
        {{ t('knowledgeBase.segmentMode') }}
        <NSelect v-model:value="uploadSegmentMode" :options="segmentModeOptions" />
        <div style="font-size: 12px; opacity: 0.7">
          {{ uploadModeTip }}
        </div>
        <!-- qa mode: format note and template download -->
        <template v-if="uploadSegmentMode === 'qa'">
          <NP depth="3" style="font-size: 12px; margin: 0;">
            {{ t('knowledgeBase.importQaTip') }}
          </NP>
          <div>
            <NButton type="primary" ghost size="small" @click="downloadQaTemplate">
              {{ t('knowledgeBase.downloadTemplate') }}
            </NButton>
          </div>
        </template>
        <template v-else-if="uploadSegmentMode === 'parent_child'">
          {{ t('knowledgeBase.childMaxChunkSize') }}
          <NInputNumber v-model:value="uploadChildMaxChunkSize" :min="50" :max="4000" style="width: 100%" />
          <span style="font-size: 12px; opacity: 0.65;">
            {{ t('knowledgeBase.childMaxChunkSizeTip') }}
          </span>
        </template>
        <NUpload
          ref="uploadRef" v-model:file-list="fileList" multiple directory-dnd
          :action="uploadAction" :accept="uploadAccept"
          :default-upload="false" :max="20" :headers="headers" @before-upload="onUploadBefore" @finish="onUploadFinish"
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
        <NFlex>
          <NButton type="primary" :disabled="!fileListLength" :loading="uploadSubmitting" @click="onUploadSubmit">
            {{ t('knowledgeBase.uploadAndGenerate') }}
          </NButton>
        </NFlex>
      </NSpace>
    </NCard>
  </NModal>

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
</template>
