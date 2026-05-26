<script setup lang='ts'>
import { computed, nextTick, onMounted, reactive, ref, watch } from 'vue'
import { NAlert, NBreadcrumb, NBreadcrumbItem, NButton, NCard, NCheckbox, NCheckboxGroup, NDataTable, NFlex, NIcon, NInput, NModal, NP, NSpace, NTag, NText, NUpload, NUploadDragger, useDialog, useMessage } from 'naive-ui'
import { ArchiveOutline } from '@vicons/ionicons5'
import { Cloud32Regular, LockClosed32Regular } from '@vicons/fluent'
import { useRoute } from 'vue-router'
import type { UploadFileInfo, UploadInst } from 'naive-ui'
import ItemEmbeddingList from './ItemEmbeddingList.vue'
import ItemGraph from './ItemGraph.vue'
import { createColumns } from './itemColumns'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore } from '@/store'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const dialog = useDialog()
const route = useRoute()
const { kbUuid: curKbUuid } = route.params as { kbUuid: string; kbId: string }
console.log('knowledge-base uuid', curKbUuid)

const showEmbeddingListModal = ref<boolean>(false)
const showGraphModal = ref<boolean>(false)
const kbItemUuidForEmbeddingList = ref<string>('')
const kbItemUuidForGraph = ref<string>('')

const modalMainHeight = ref<number>(500)
const tableMaxHeight = ref<number>(500)
const loading = ref<boolean>(false)
const submitting = ref<boolean>(false)
const showItemEditModal = ref<boolean>(false)
const showUploadModal = ref<boolean>(false)
const showIndexModal = ref<boolean>(false)
const itemList = ref<KnowledgeBase.Item[]>([])
const indexAfterUpload = ref(false)
const indexTypeSelected = ref<string[]>(['embedding'])
const uploadRef = ref<UploadInst | null>(null)
const headers = { Authorization: '' }
const fileListLength = ref(0)
const fileList = ref<UploadFileInfo[]>([])
const paginationReactive = reactive({
  page: 1,
  pageSize: 10,
  itemCount: 0,
})
const searchValue = ref<string>('')
const tmpItem = reactive<KnowledgeBase.Item>(knowledgeBaseEmptyItem())
// 控制 input 按钮
const inputStatus = computed(() => tmpItem.title.trim().length < 1 && !submitting.value)
const { isMobile } = useBasicLayout()
const authStore = useAuthStore()
const token = ref<string>(authStore.token)
const checkedItemRowKeys = ref<string[]>([])
const checkedItems = ref<KnowledgeBase.Item[]>([])
const curKnowledgeBase: KnowledgeBase.Info = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())

// 文件预览
const showFileContentModal = ref<boolean>(false)
const previewFileUrl = ref<string>('')
const previewMimeType = ref<string>('')
const previewFileContent = ref<string>('')
const previewFileName = ref<string>('')

const openFileInNewTab = function (url: string) {
  const x = new window.XMLHttpRequest()
  x.open('GET', url, true)
  x.responseType = 'blob'
  x.onload = () => {
    const url = window.URL.createObjectURL(x.response)
    const a = document.createElement('a')
    a.href = url
    a.download = previewFileName.value
    a.click()
  }
  x.send()
}

const showFileContent = (selected: KnowledgeBase.Item = knowledgeBaseEmptyItem()) => {
  // window.open(`/api${selected.sourceFileUrl}?token=${token.value}`, '_blank')
  previewFileContent.value = ''
  previewFileName.value = ''
  previewFileUrl.value = `${selected.sourceFileUrl}?token=${token.value}`
  previewFileName.value = selected.sourceFileName
  console.log('previewFileUrl', previewFileUrl.value)
  const ext = selected.sourceFileName.substring(selected.sourceFileName.lastIndexOf('.') + 1)
  switch (ext) {
    case 'pdf':
      previewMimeType.value = 'application/pdf'
      break
    case 'doc':
    case 'docx':
      previewMimeType.value = 'application/msword'
      break
    case 'ppt':
    case 'pptx':
      previewMimeType.value = 'application/vnd.ms-powerpoint'
      break
    case 'xls':
    case 'xlsx':
      previewMimeType.value = 'application/vnd.ms-excel'
      break
    case 'html':
      previewMimeType.value = 'text/html'
      break
    case 'txt':
      previewMimeType.value = 'text/plain'
      api.loadFileContent(previewFileUrl.value).then((resp) => {
        console.log('loadFileContent', resp)
        previewFileContent.value = resp.data
      }).catch((err) => {
        console.error('loadFileContent error', err)
      })
      break
    default:
      previewMimeType.value = 'text/plain'
  }
  showFileContentModal.value = true
}

const showEmbeddingList = (selected: KnowledgeBase.Item = knowledgeBaseEmptyItem()) => {
  showEmbeddingListModal.value = true
  kbItemUuidForEmbeddingList.value = selected.uuid
}

const showGraph = (selected: KnowledgeBase.Item = knowledgeBaseEmptyItem()) => {
  showGraphModal.value = true
  kbItemUuidForGraph.value = selected.uuid
}

const changeEditModal = (selected: KnowledgeBase.Item = knowledgeBaseEmptyItem()) => {
  if (selected.kbId !== '0') {
    Object.assign(tmpItem, selected)
  } else {
    Object.assign(tmpItem, knowledgeBaseEmptyItem())
    tmpItem.kbId = curKnowledgeBase.id
    tmpItem.kbUuid = curKnowledgeBase.uuid
  }
  showItemEditModal.value = !showItemEditModal.value
}

function rowKey(row: KnowledgeBase.Item) {
  return row.uuid
}

const columns = createColumns(showEmbeddingList, showGraph, showFileContent, changeEditModal, deleteKbItem)

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
    kbItemUuidForGraph.value = ''
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
  const itemMap = new Map<string, KnowledgeBase.Item>()
  const tmpItems = [] as KnowledgeBase.Item[]
  tmpItems.push(...(rows as KnowledgeBase.Item[]))
  tmpItems.push(...checkedItems.value)
  tmpItems.forEach((item) => {
    if (item)
      itemMap.set(item.uuid, item)
  })
  checkedItems.value = Array.from(itemMap.entries())
    .filter(([key]) => checkedItemRowKeys.value.includes(key))
    .map(([, value]) => value)
}

function removeCheckedItem(item: KnowledgeBase.Item) {
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
  return true
}

function onUploadChange(options: { fileList: UploadFileInfo[] }) {
  console.log('onUploadChange')
  fileListLength.value = options.fileList.length
}

function onUploadSubmit() {
  uploadRef.value?.submit()
  setTimeout(() => {
    showUploadModal.value = false
    search(1)
  }, 3000)
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
    ms.error(t('knowledgeBase.uploadFailedResponseError'))
    return
  }
  const { success, message } = respData
  console.log('onUploadFinish', success, message)
  if (success)
    ms.success(t('common.uploadSuccess'))
  else
    ms.error(message || t('common.uploadFailed'))

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

async function saveOrUpdate() {
  try {
    submitting.value = true
    const resp = await api.knowledgeBaseItemSaveOrUpdate<KnowledgeBase.Item>(tmpItem)
    Object.assign(tmpItem, resp.data)
  } finally {
    submitting.value = false
    showItemEditModal.value = false
    Object.assign(tmpItem, knowledgeBaseEmptyItem())
    search(1)
  }
}

function deleteKbItem(row: KnowledgeBase.Item) {
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

async function initData() {
  search(1)
  const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(curKbUuid)
  Object.assign(curKnowledgeBase, resp.data)
}

onMounted(async () => {
  modalMainHeight.value = window.innerHeight - 150
  tableMaxHeight.value = window.innerHeight - 420
  if (curKnowledgeBase.title === '')
    await initData()
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
        <NIcon v-if="curKnowledgeBase.isPublic" :component="Cloud32Regular" />
        <NIcon v-if="!curKnowledgeBase.isPublic" :component="LockClosed32Regular" />
      </template>
      {{ curKnowledgeBase.remark }}
    </NCard>
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.generatedKnowledge')" hoverable>
      <div class="flex gap-3 mb-4" :class="[isMobile ? 'flex-col' : 'flex-row justify-between']">
        <div class="flex items-left gap-2">
          <NButton type="primary" size="small" @click="changeEditModal()">
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

  <NModal
    v-model:show="showItemEditModal" style="width: 90%" preset="card"
    :title="t('knowledgeBase.knowledgeItemAddEdit')"
  >
    <NSpace vertical>
      {{ t('store.title') }}
      <NInput v-model:value="tmpItem.title" maxlength="100" show-count />
      {{ t('knowledgeBase.brief') }}
      <NInput v-model:value="tmpItem.brief" type="textarea" show-count :autosize="{ minRows: 2, maxRows: 5 }" />
      {{ t('common.content') }}
      <NInput v-model:value="tmpItem.remark" type="textarea" show-count :rows="10" />
    </NSpace>
    <template #footer>
      <div class="flex justify-end">
        <NButton type="primary" :disabled="inputStatus" @click="() => { saveOrUpdate() }">
          {{ t('common.confirm') }}
        </NButton>
      </div>
    </template>
  </NModal>

  <!-- Upload files -->
  <NModal v-model:show="showUploadModal" style="width: 90%;  min-height: 700px;" preset="card" :title="t('knowledgeBase.knowledgeItemUpload')">
    <NCard style="margin-top: 12px" :title="t('knowledgeBase.uploadDocToGenerate')" hoverable>
      <NSpace vertical>
        <NUpload
          ref="uploadRef" multiple :default-file-list="fileList" directory-dnd
          :action="`/api/knowledge-base/upload/${curKbUuid}?indexAfterUpload=${indexAfterUpload}`"
          :default-upload="false" :max="20" :headers="headers" @before-upload="onUploadBefore" @finish="onUploadFinish"
          @change="onUploadChange"
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
              {{ t('knowledgeBase.supportedFileFormats') }}<br>
              {{ t('knowledgeBase.fileSizeLimit') }}
            </NP>
          </NUploadDragger>
        </NUpload>
        <NFlex>
          <NButton type="primary" :disabled="!fileListLength" @click="onUploadSubmit">
            {{ t('knowledgeBase.uploadAndGenerate') }}
          </NButton>
        </NFlex>
      </NSpace>
    </NCard>
  </NModal>

  <NModal v-model:show="showEmbeddingListModal" style="width: 90%; " preset="card" :title="t('knowledgeBase.embeddingList')">
    <ItemEmbeddingList :kb-item-uuid="kbItemUuidForEmbeddingList" />
  </NModal>
  <NModal v-model:show="showGraphModal" style="width: 90%;" display-directive="show" preset="card" :title="t('knowledgeBase.graphLabel')">
    <ItemGraph :kb-item-uuid="kbItemUuidForGraph" />
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
  <NModal v-model:show="showFileContentModal" style="width: 90%; " preset="card" :title="`${t('workflow.filePreviewTitle')}${previewFileName}`">
    <div style="text-align: center;max-height:700px;overflow-y: auto">
      <div v-if="previewFileUrl && previewMimeType === 'text/plain'">
        {{ previewFileContent }}
      </div>
      <object
        v-if="previewFileUrl && previewMimeType !== 'text/plain' && previewMimeType !== 'application/pdf'"
        :data="previewFileUrl" width="100%" height="90%" :type="previewMimeType"
      >
        <p>{{ t('workflow.browserNotSupportEmbed') }}</p>
      </object>
    </div>
    <template #footer>
      <NButton type="primary" text tag="a" size="small" @click="openFileInNewTab(previewFileUrl)">
        {{ t('workflow.clickToDownload') }}{{ previewFileName }}
      </NButton>
    </template>
  </NModal>
</template>
