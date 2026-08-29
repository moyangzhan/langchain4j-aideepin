<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { computed, h, onActivated, onMounted, reactive, ref, watch } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NButton, NDataTable, NInput, useDialog, useMessage } from 'naive-ui'
import { RouterLink, useRouter } from 'vue-router'
import { ApiKeyModal } from '@/components/common'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore, useKbStore } from '@/store'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const router = useRouter()
const dialog = useDialog()
const ms = useMessage()
const loading = ref(false)
const tableMaxHeight = ref<number>(500)
const infoList = ref<KnowledgeBase.Info[]>([])
const paginationReactive = reactive({
  page: 1,
  pageSize: 20,
  itemCount: 0,
  prefix: () => t('common.total', { n: paginationReactive.itemCount }),
})
const searchValue = ref<string>('')
const { isMobile } = useBasicLayout()
const authStore = useAuthStore()
const kbStore = useKbStore()
const token = ref<string>(authStore.token)
const showApiKeyModal = ref(false)
const activeKb = ref<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())

// 仅从 KB 编辑/新建页返回时刷新列表，其余返回复用 KeepAlive 缓存
// Refresh the list only when returning from the KB edit/add pages; other returns reuse the KeepAlive cache
const cameFromRoute = ref('')
router.afterEach((to, from) => {
  if (to.name === 'KnowledgeBaseManage')
    cameFromRoute.value = (from.name as string) || ''
})
onActivated(() => {
  if (['KnowledgeBaseAdd', 'KnowledgeBaseEdit'].includes(cameFromRoute.value))
    search(paginationReactive.page)
})
// 序号列宽度按总条数位数自适应；下限按 3 位数计算——旧下限 40px 扣除单元格内边距后
// 连两位数都放不下会换行
// Serial-number column width auto-fits to the digit count of total rows, with a floor
// wide enough for 3 digits (the old 40px floor made two-digit numbers wrap)
const serialColWidth = computed(() => {
  const digits = Math.max(String(Math.max(paginationReactive.itemCount, 1)).length, 3)
  return digits * 10 + 28
})

// table相关
const createColumns = (): DataTableColumns<KnowledgeBase.Info> => {
  return [
    {
      title: '#',
      key: 'serialNumber',
      width: serialColWidth.value,
      align: 'center',
      render(_row, index) {
        return (paginationReactive.page - 1) * paginationReactive.pageSize + index + 1
      },
    },
    {
      title: t('common.title'),
      key: 'title',
      width: 200,
      render(row) {
        return h(
          RouterLink,
          {
            class: 'hljs-link',
            to: {
              name: 'KnowledgeBaseManageDetail',
              params: {
                kbUuid: row.uuid,
              },
            },
          },
          { default: () => row.title },
        )
      },
    },
    {
      title: t('common.description'),
      key: 'remark',
    },
    {
      title: t('knowledgeBase.isPublic'),
      key: 'isPublic',
      width: 100,
      render(row) {
        return row.isPublic ? t('common.yes') : t('common.no')
      },
    },
    {
      title: t('knowledgeBase.isStrict'),
      key: 'isStrict',
      width: 100,
      render(row) {
        return row.isStrict ? t('common.yes') : t('common.no')
      },
    },
    {
      title: t('common.action'),
      key: 'actions',
      width: 100,
      align: 'center',
      render(row) {
        return h('div', { class: 'grid gap-1' }, {
          default: () => [
            h('div', { class: 'flex gap-1' }, [
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'tiny',
                  type: 'info',
                  onClick: () => router.push({ name: 'KnowledgeBaseManageDetail', params: { kbUuid: row.uuid } }),
                },
                { default: () => t('knowledgeBase.docs') },
              ),
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'tiny',
                  type: 'info',
                  onClick: () => {
                    activeKb.value = row
                    showApiKeyModal.value = true
                  },
                },
                { default: () => t('extApi.apiAccess') },
              ),
            ]),
            h('div', { class: 'flex gap-1' }, [
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'tiny',
                  type: 'info',
                  onClick: () => router.push({ name: 'KnowledgeBaseEdit', params: { kbUuid: row.uuid } }),
                },
                { default: () => t('common.edit') },
              ),
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'tiny',
                  type: 'error',
                  onClick: () => deleteKb(row),
                },
                { default: () => t('common.delete') },
              ),
            ]),
          ],
        })
      },
    },
  ]
}

const columns = computed(() => createColumns())

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
  if (loading.value) {
    ms.warning(t('common.loadingPleaseWait'), {
      duration: 2000,
    })
    return
  }
  loading.value = true
  try {
    const resp = await api.knowledgeBaseSearchMine<KnowledgeBase.InfoListResp>(searchValue.value, currentPage, paginationReactive.pageSize)
    infoList.value = resp.data.records
    paginationReactive.page = currentPage
    paginationReactive.itemCount = resp.data.total
  } finally {
    loading.value = false
  }
}

function deleteKb(row: KnowledgeBase.Info) {
  dialog.warning({
    title: t('common.tip'),
    content: t('knowledgeBase.deleteKbConfirm', { title: row.title }),
    positiveText: t('common.confirm'),
    negativeText: t('common.cancel'),
    onPositiveClick: () => {
      api.knowledgeBaseDelete(row.uuid)
      const index = infoList.value.findIndex(item => item.uuid === row.uuid)
      if (index !== -1)
        infoList.value.splice(index, 1)
      ms.success(t('common.deleteSuccess'))
    },
  })
}

async function initData() {
  search(1)
}

watch(
  () => token,
  () => {
    if (token.value)
      initData()
  },
  { immediate: true },
)

onMounted(() => {
  tableMaxHeight.value = window.innerHeight - 220
})
</script>

<template>
  <div class="flex flex-col w-full p-4">
    <NBreadcrumb separator=">">
      <NBreadcrumbItem href="/">
        {{ t('common.home') }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :href="`#/qa/${kbStore.activeKbUuid}`">
        {{ t('menu.knowledgeBase') }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :clickable="false">
        {{ t('knowledgeBase.myKnowledgeBase') }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <div class="flex gap-3 mb-2 mt-1" :class="[isMobile ? 'flex-col' : 'flex-row justify-between']">
      <div class="flex items-center space-x-4">
        <NButton type="primary" size="small" @click="router.push({ name: 'KnowledgeBaseAdd' })">
          {{ t('common.add') }}
        </NButton>
      </div>
      <div class="flex justify-between">
        <NInput v-model:value="searchValue" style="width: 100%" @keyup="onKeyUpSearch" />
        <NButton type="primary" ghost @click="search(1)">
          {{ t('common.search') }}
        </NButton>
      </div>
    </div>
    <NDataTable
      remote :loading="loading" :max-height="tableMaxHeight" :columns="columns" :data="infoList" :pagination="paginationReactive"
      :single-line="false" :bordered="true" @update:page="onHandlePageChange"
    />
  </div>

  <ApiKeyModal v-model:show="showApiKeyModal" type="knowledge" :uuid="activeKb.uuid" :title="activeKb.title" />
</template>
