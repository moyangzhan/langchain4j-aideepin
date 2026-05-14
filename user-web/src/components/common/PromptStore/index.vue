<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { computed, h, reactive, ref, watch } from 'vue'
import { NAlert, NButton, NDataTable, NIcon, NInput, NInputGroup, NList, NListItem, NModal, NSpace, NThing, useMessage } from 'naive-ui'
import { Note24Regular } from '@vicons/fluent'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'

interface Props {
  visible: boolean
}
interface Emit {
  (e: 'update:visible', visible: boolean): void
}
const props = defineProps<Props>()
const emit = defineEmits<Emit>()
const authStore = useAuthStore()
const token = ref<string>(authStore.token)
const message = useMessage()
const show = computed({
  get: () => props.visible,
  set: (visible: boolean) => emit('update:visible', visible),
})
const loading = ref(false)
const showModal = ref(false)
const exportLoading = ref(false)
const searchValue = ref<string>('')
// 移动端自适应相关
const { isMobile } = useBasicLayout()
const promptList = ref<Chat.Prompt[]>([])
const paginationReactive = reactive({
  page: 1,
  pageSize: 10,
  itemCount: 0,
})
// 用于添加修改的临时prompt参数
const tmpPromptKey = ref('')
const tmpPromptValue = ref('')
const tmpPromptId = ref(0)
// Modal模式，根据不同模式渲染不同的Modal内容
const modalMode = ref('')
// 这个是为了后期的修改Prompt内容考虑，因为要针对无uuid的list进行修改，且考虑到不能出现标题和内容的冲突，所以就需要一个临时item来记录一下
const tempModifiedItem = ref<any>({})
// 添加修改导入都使用一个Modal, 临时修改内容占用tempPromptKey,切换状态前先将内容都清楚
const changeShowModal = (mode: 'add' | 'modify' | 'local_import', selected = { act: '', prompt: '', id: 0 }) => {
  if (mode === 'add') {
    tmpPromptKey.value = ''
    tmpPromptValue.value = ''
  } else if (mode === 'modify') {
    tempModifiedItem.value = { ...selected }
    tmpPromptId.value = selected.id
    tmpPromptKey.value = selected.act
    tmpPromptValue.value = selected.prompt
  } else if (mode === 'local_import') {
    tmpPromptKey.value = 'local_import'
    tmpPromptValue.value = ''
  }
  showModal.value = !showModal.value
  modalMode.value = mode
}

// 控制 input 按钮
const inputStatus = computed(() => tmpPromptKey.value.trim().length < 1 || tmpPromptValue.value.trim().length < 1)

// Prompt模板相关操作
const addPromptTemplate = () => {
  for (const i of promptList.value) {
    if (i.act === tmpPromptKey.value) {
      message.error(t('store.addRepeatTitleTips'))
      return
    }
    if (i.prompt === tmpPromptValue.value) {
      message.error(t('store.addRepeatContentTips', { msg: tmpPromptKey.value }))
      return
    }
  }
  api.promptsSave([{ act: tmpPromptKey.value, prompt: tmpPromptValue.value }])
  message.success(t('common.addSuccess'))
  changeShowModal('add')
}

const modifyPromptTemplate = () => {
  let index = 0

  // 通过临时索引把待修改项摘出来
  for (const i of promptList.value) {
    if (i.act === tempModifiedItem.value.key && i.prompt === tempModifiedItem.value.value)
      break
    index = index + 1
  }

  const tempList = promptList.value.filter((_: any, i: number) => i !== index)

  // 搜索有冲突的部分
  for (const i of tempList) {
    if (i.act === tmpPromptKey.value) {
      message.error(t('store.editRepeatTitleTips'))
      return
    }
    if (i.prompt === tmpPromptValue.value) {
      message.error(t('store.editRepeatContentTips', { msg: i.act }))
      return
    }
  }

  promptList.value = [{ id: tmpPromptId.value, act: tmpPromptKey.value, prompt: tmpPromptValue.value, renderKey: tmpPromptKey.value, renderValue: tmpPromptValue.value }, ...tempList]
  message.success(t('common.editSuccess'))
  changeShowModal('modify')
  api.promptEdit(tmpPromptId.value, tempModifiedItem.value.key, tempModifiedItem.value.value)
}

const deletePromptTemplate = (row: { id: number; act: string; prompt: string }) => {
  promptList.value = [
    ...promptList.value.filter((item: { act: string; prompt: string }) => item.act !== row.act),
  ] as never
  message.success(t('common.deleteSuccess'))
  api.promptDel(row.id)
}

const importPromptTemplate = async (from = 'online') => {
  try {
    const jsonData = JSON.parse(tmpPromptValue.value)
    let key = ''
    let value = ''
    // 可以扩展加入更多模板字典的key
    if ('key' in jsonData[0]) {
      key = 'key'
      value = 'value'
    } else if ('act' in jsonData[0]) {
      key = 'act'
      value = 'prompt'
    } else {
      // 不支持的字典的key防止导入 以免破坏prompt商店打开
      message.warning('prompt key not supported.')
      throw new Error('prompt key not supported.')
    }
    const prompts = []
    for (const i of jsonData) {
      if (!(key in i) || !(value in i))
        throw new Error(t('store.importError'))
      prompts.push({ act: i[key], prompt: i[value] })
    }
    await api.promptsSave(prompts)
    message.success(t('common.importSuccess'))
    const resp = await api.searchPrompts<PageResponse>(1, 20)
    setResp(1, resp.data)
  } catch (error) {
    console.error(error)
    message.error(t('store.jsonFormatError'))
  }
  if (from === 'local')
    showModal.value = !showModal.value
}

// 模板导出
const exportPromptTemplate = async () => {
  exportLoading.value = true
  try {
    const resp = await api.searchPrompts<PageResponse>(1, 10000)
    const jsonDataStr = JSON.stringify(resp.data)
    const blob = new Blob([jsonDataStr], { type: 'application/json' })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = 'export_prompts.json'
    link.click()
    URL.revokeObjectURL(url)
  } finally {
    exportLoading.value = false
  }
}

// 移动端自适应相关
const renderTemplate = () => {
  const [keyLimit, valueLimit] = isMobile.value ? [10, 30] : [15, 50]

  return promptList.value.map((item: { id: number; act: string; prompt: string }) => {
    return {
      renderKey: item.act.length <= keyLimit ? item.act : `${item.act.substring(0, keyLimit)}...`,
      renderValue: item.prompt.length <= valueLimit ? item.prompt : `${item.prompt.substring(0, valueLimit)}...`,
      act: item.act,
      prompt: item.prompt,
      id: item.id,
    }
  })
}

// table相关
const createColumns = (): DataTableColumns<Chat.Prompt> => {
  return [
    {
      title: t('store.title'),
      key: 'renderKey',
    },
    {
      title: t('store.description'),
      key: 'renderValue',
    },
    {
      title: t('common.action'),
      key: 'actions',
      width: 100,
      align: 'center',
      render(row) {
        return h('div', { class: 'flex items-center flex-col gap-2' }, {
          default: () => [h(
            NButton,
            {
              tertiary: true,
              size: 'small',
              type: 'info',
              onClick: () => changeShowModal('modify', row),
            },
            { default: () => t('common.edit') },
          ),
          h(
            NButton,
            {
              tertiary: true,
              size: 'small',
              type: 'error',
              onClick: () => deletePromptTemplate(row),
            },
            { default: () => t('common.delete') },
          ),
          ],
        })
      },
    },
  ]
}

const columns = createColumns()

const dataSource = computed(() => {
  return renderTemplate()
})

async function handlePageChange(currentPage: number) {
  loading.value = true
  const resp = await api.searchPrompts<PageResponse>(currentPage, paginationReactive.pageSize, searchValue.value)
  setResp(currentPage, resp.data)
  loading.value = false
}

async function search(event: KeyboardEvent) {
  if (event.key === 'Enter' && !event.shiftKey) {
    event.preventDefault()
    clickSearch()
  }
}

async function clickSearch() {
  const resp = await api.searchPrompts<PageResponse>(1, paginationReactive.pageSize, searchValue.value)
  setResp(1, resp.data)
}

async function initPrompts() {
  const resp = await api.searchPrompts<PageResponse>(1, paginationReactive.pageSize)
  setResp(1, resp.data)
}

function setResp(currentPage: number, data: PageResponse) {
  promptList.value = data.records
  paginationReactive.page = currentPage
  paginationReactive.itemCount = data.total
}

watch(
  () => token,
  () => {
    if (token.value)
      initPrompts()
  },
  { immediate: true },
)
</script>

<template>
  <NModal v-model:show="show" style="width: 90%; max-width: 900px;" preset="card">
    <div class="space-y-4">
      <div class="flex gap-3 mb-4" :class="[isMobile ? 'flex-col' : 'flex-row justify-between']">
        <div class="flex items-center space-x-4">
          <NButton type="primary" size="small" @click="changeShowModal('add')">
            {{ t('common.add') }}
          </NButton>
          <NButton size="small" @click="changeShowModal('local_import')">
            {{ t('common.import') }}
          </NButton>
          <NButton size="small" :loading="exportLoading" @click="exportPromptTemplate()">
            {{ t('common.export') }}
          </NButton>
        </div>
        <div class="flex items-center">
          <NInputGroup>
            <NInput v-model:value="searchValue" style="width: 100%" @keyup="search" />
            <NButton ghost @click="clickSearch">
              {{ t('common.search') }}
            </NButton>
          </NInputGroup>
        </div>
      </div>
      <NDataTable
        v-if="!isMobile" remote :loading="loading" :max-height="400" :columns="columns" :data="dataSource"
        :pagination="paginationReactive" :bordered="false" @update:page="handlePageChange"
      />
      <NList v-if="isMobile" style="max-height: 400px; overflow-y: auto;">
        <NListItem v-for="(item, index) of dataSource" :key="index">
          <NThing :title="item.renderKey" :description="item.renderValue" />
          <template #suffix>
            <div class="flex flex-col items-center gap-2">
              <NButton tertiary size="small" type="info" @click="changeShowModal('modify', item)">
                {{ t('common.edit') }}
              </NButton>
              <NButton tertiary size="small" type="error" @click="deletePromptTemplate(item)">
                {{ t('common.delete') }}
              </NButton>
            </div>
          </template>
        </NListItem>
      </NList>
    </div>
  </NModal>

  <NModal v-model:show="showModal" style="width: 90%; max-width: 600px;" preset="card">
    <NSpace v-if="modalMode === 'add' || modalMode === 'modify'" vertical>
      {{ t('store.title') }}
      <NInput v-model:value="tmpPromptKey" />
      {{ t('store.description') }}
      <NInput v-model:value="tmpPromptValue" type="textarea" :autosize="{ minRows: 10, maxRows: 40 }" />
      <NButton
        block type="primary" :disabled="inputStatus"
        @click="() => { modalMode === 'add' ? addPromptTemplate() : modifyPromptTemplate() }"
      >
        {{ t('common.confirm') }}
      </NButton>
    </NSpace>
    <NSpace v-if="modalMode === 'local_import'" vertical>
      <NInput
        v-model:value="tmpPromptValue" :placeholder="t('store.importPlaceholder')"
        :autosize="{ minRows: 10, maxRows: 50 }" type="textarea"
      />
      <NButton block type="primary" :disabled="inputStatus" @click="() => { importPromptTemplate('local') }">
        {{ t('common.import') }}
      </NButton>
      <NAlert :title="t('store.example')">
        <template #icon>
          <NIcon>
            <Note24Regular />
          </NIcon>
        </template>
        [<br>
        &nbsp;&nbsp;{<br>
        &nbsp;&nbsp;&nbsp;&nbsp;"act":"充当英语翻译和改进者",<br>
        &nbsp;&nbsp;&nbsp;&nbsp;"prompt": "我希望你能担任英语翻译、拼写校对和修辞改进的角色。"<br>
        &nbsp;&nbsp;}<br>
        ]
      </NAlert>
    </NSpace>
  </NModal>
</template>
