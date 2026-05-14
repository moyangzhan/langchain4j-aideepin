<script setup lang='ts'>
import { h, reactive, ref, watch } from 'vue'
import { NButton, NDataTable, NInput, NTag, useMessage } from 'naive-ui'
import type { DataTableColumns, DataTableRowData, DataTableRowKey } from 'naive-ui'
import { defaultConv } from '@/store/modules/chat/helper'
import { useChatStore, useUserStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
interface Props {
  conversation: Chat.Conversation
  tmpSave: boolean // 是否临时保存(即不保存到远程)
}
const props = withDefaults(defineProps<Props>(), {
  conversation: () => defaultConv(),
})
const emit = defineEmits<Emit>()
interface Emit {
  (e: 'selectedChanged', knowledgeIds: string[], knowledgeList: Chat.ConvKnowledge[]): void
  (e: 'submitted', knowledgeIds: string[], knowledgeList: Chat.ConvKnowledge[]): void
}
const ms = useMessage()
const userStore = useUserStore()
const chatStore = useChatStore()
const loading = ref(false)
const knowledgeList = ref<KnowledgeBase.Info[]>([])
const paginationReactive = reactive({
  page: 1,
  pageSize: 10,
  itemCount: 0,
})
const tmpConvKnowledgeList = ref<Chat.ConvKnowledge[]>([])
const tmpKnowledgeIds = ref<string[]>([])
const cacheRows = ref<KnowledgeBase.Info[]>([])
const searchValue = ref<string>('')
const checkedRowKeysRef = ref<DataTableRowKey[]>([])

// table相关
const createColumns = (): DataTableColumns<KnowledgeBase.Info> => {
  return [
    {
      type: 'selection',
    },
    {
      title: t('chat.convKnowledgeTitle'),
      key: 'title',
      width: 200,
    },
    {
      title: t('chat.convKnowledgeDescription'),
      key: 'remark',
    },
    {
      title: t('chat.convKnowledgeAttribute'),
      key: 'remark',
      render(row) {
        return h('div', { class: 'flex items-center space-x-1' }, {
          default: () => [h(
            NTag,
            {
              tertiary: true,
              size: 'small',
            },
            { default: () => row.ownerName === userStore.userInfo?.name ? t('common.mine') : row.ownerName },
          ),
          h(
            NTag,
            {
              tertiary: true,
              size: 'small',
            },
            { default: () => row.isPublic ? t('common.public') : t('common.private') },
          ),
          ],
        })
      },
    },
  ]
}

const columns = createColumns()

function handleClose(knowledgeId: string) {
  const index = tmpConvKnowledgeList.value.findIndex(kb => kb.id === knowledgeId)
  if (index !== -1) {
    tmpConvKnowledgeList.value.splice(index, 1)
    const idIndex = tmpKnowledgeIds.value.findIndex(id => id === knowledgeId)
    if (idIndex !== -1)
      tmpKnowledgeIds.value.splice(idIndex, 1)
  }
  emit('selectedChanged', tmpKnowledgeIds.value, tmpConvKnowledgeList.value)
}

async function onHandlePageChange(page: number) {
  search(page)
}

async function onKeyUpSearch(event: KeyboardEvent) {
  if (event.key === 'Enter' && !event.shiftKey) {
    event.preventDefault()
    search(1)
  }
}

async function search(currentPage: number) {
  if (loading.value) {
    ms.warning(t('common.loadingPleaseWait'), {
      duration: 2000,
    })
    return
  }
  loading.value = true
  try {
    const resp = await api.knowledgeBaseSearchMine<KnowledgeBase.InfoListResp>(searchValue.value, currentPage, paginationReactive.pageSize, true)
    knowledgeList.value = resp.data.records
    paginationReactive.page = currentPage
    paginationReactive.itemCount = resp.data.total
  } finally {
    loading.value = false
  }
}

function onHandleCheck(rowKeys: DataTableRowKey[], rows: DataTableRowData[]) {
  checkedRowKeysRef.value = rowKeys
  const map = new Map()
  const selectedRows = (rows as KnowledgeBase.Info[]).filter(item => !!item)
  cacheRows.value = cacheRows.value.concat(selectedRows).filter(item => !map.has(item.id) && map.set(item.id, 0))
  tmpConvKnowledgeList.value = cacheRows.value.filter((item: KnowledgeBase.Info) => rowKeys.includes(item.id)).map(knowledge => ({
    id: knowledge.id,
    uuid: knowledge.uuid,
    title: knowledge.title,
    isMine: knowledge.ownerUuid === userStore.userInfo.uuid,
    isPublic: knowledge.isPublic,
    kbInfo: knowledge,
    isEnable: true,
  }))
  emit('selectedChanged', tmpKnowledgeIds.value, tmpConvKnowledgeList.value)
}

async function handleSubmit() {
  try {
    await api.convEdit(props.conversation.uuid, {
      kbIds: tmpKnowledgeIds.value,
    })
    chatStore.updateConv(props.conversation.uuid, { kbIds: tmpKnowledgeIds.value, convKnowledgeList: tmpConvKnowledgeList.value })
    ms.success(t('chat.convKnowledgeSaved'), {
      duration: 3000,
    })
  } catch (error) {
    console.error('handleSaveKnowledge error', error)
  }
  emit('submitted', tmpKnowledgeIds.value, tmpConvKnowledgeList.value)
}

watch(() => props.conversation.kbIds, (newVal) => {
  console.log('watch newVal', newVal)
  tmpConvKnowledgeList.value = props.conversation.convKnowledgeList || []
  tmpKnowledgeIds.value = newVal || []

  if (knowledgeList.value.length === 0)
    search(1)
}, { immediate: true, deep: true })
</script>

<template>
  <div class="flex flex-col space-y-2">
    <div class="mb-2">
      <NTag
        v-for="convKnowledge in tmpConvKnowledgeList" :key="convKnowledge.uuid" closable class="mr-2"
        @close="handleClose(convKnowledge.id)"
      >
        {{ convKnowledge.title }}
      </NTag>
      <div v-if="tmpConvKnowledgeList.length === 0">
        {{ t('chat.noSelectedData') }}
      </div>
    </div>
    <div>
      <div class="flex justify-between">
        <NInput v-model:value="searchValue" class="mr-2" :placeholder="t('chat.searchTitlePlaceholder')" clearable @keyup="onKeyUpSearch" />
        <NButton type="primary" ghost @click="search(1)">
          {{ t('common.search') }}
        </NButton>
      </div>
      <NDataTable
        v-model:checked-row-keys="tmpKnowledgeIds" remote striped :loading="loading" :columns="columns"
        :data="knowledgeList" :pagination="paginationReactive" :single-line="false" :row-key="(row) => row.id"
        :bordered="true" @update:page="onHandlePageChange" @update:checked-row-keys="onHandleCheck"
      />
      <div v-if="!tmpSave" class="flex justify-end mt-4">
        <NButton type="primary" @click="handleSubmit">
          {{ t('common.save') }}
        </NButton>
      </div>
    </div>
  </div>
</template>
