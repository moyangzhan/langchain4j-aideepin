<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { NDataTable, NModal, NScrollbar } from 'naive-ui'
import { h, reactive, ref, watch } from 'vue'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  kbItemUuid: string
}
const props = defineProps<Props>()
const infoList = ref<KnowledgeBase.KbEmbedding[]>([])
const showModal = ref<boolean>(false)
const modalContent = ref<string>('')
const loading = ref(false)
const paginationReactive = reactive({
  page: 1,
  pageSize: 10,
  itemCount: 0,
})
// table相关
const createColumns = (): DataTableColumns<KnowledgeBase.KbEmbedding> => {
  return [
    {
      title: 'ID',
      key: 'embeddingId',
      width: 200,
    },
    {
      title: t('knowledgeBase.embedding'),
      key: 'embedding',
      render(row) {
        return h('div', {
          class: 'flex flex-col',
          style: 'cursor: pointer;',
          onClick: () => {
            showModal.value = true
            modalContent.value = row.embedding.join(',')
          },
        }, {
          default: () => {
            if (row.embedding && row.embedding.length > 8)
              return `${row.embedding.slice(0, 8).join(',')}...`

            return row.embedding.join(',')
          },
        })
      },
    },
    {
      title: t('knowledgeBase.docFragment'),
      key: 'text',
      render(row) {
        return h('div', {
          class: 'flex flex-col',
          style: 'cursor: pointer;',
          onClick: () => {
            showModal.value = true
            modalContent.value = row.text
          },
        }, {
          default: () => {
            if (row.text.length > 80)
              return `${row.text.substring(0, 50)}...`

            return row.text
          },
        })
      },
    },
  ]
}

const columns = createColumns()

async function loadList(currentPage: number) {
  loading.value = true
  try {
    const resp = await api.knowledgeBaseEmbedding<PageResponse>(props.kbItemUuid, currentPage, paginationReactive.pageSize)
    infoList.value = resp.data.records
    paginationReactive.page = currentPage
    paginationReactive.itemCount = resp.data.total
  } finally {
    loading.value = false
  }
}

async function onHandlePageChange(currentPage: number) {
  loadList(currentPage)
}

watch(
  () => props.kbItemUuid,
  () => {
    if (props.kbItemUuid)
      loadList(1)
  },
  { immediate: true },
)
</script>

<template>
  <NDataTable
    remote :loading="loading" :max-height="400" :columns="columns" :data="infoList"
    :pagination="paginationReactive" :single-line="false" :bordered="true" @update:page="onHandlePageChange"
  />

  <NModal v-model:show="showModal" style="width: 60%;" preset="card" :title="t('common.detail')">
    <NScrollbar style="max-height: 400px">
      {{ modalContent }}
    </NScrollbar>
  </NModal>
</template>
