<script setup lang="ts">
import { h, nextTick, ref, watch } from 'vue'
import { NIcon, NSelect } from 'naive-ui'
import { Cloud32Regular, LockClosed32Regular } from '@vicons/fluent'
import type { SelectGroupOption, SelectInst, SelectOption } from 'naive-ui'
import type { VNodeChild } from 'vue'
import { debounce } from '@/utils/functions/debounce'
import { useAuthStore, useUserStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  knowledgeBaseUuid: string
}
interface Emit {
  (e: 'selected', knowledge_base_uuid: string, knowledge_base_name: string): void
}
const props = withDefaults(defineProps<Props>(), {
  knowledgeBaseUuid: '',
})
const emit = defineEmits<Emit>()
const selectInstRef = ref<SelectInst | null>(null)
const authStore = useAuthStore()
const userStore = useUserStore()
const selectedKnowledgeUuid = ref<string>(props.knowledgeBaseUuid)
const currentPage = ref<number>(1)
const pageSize = 10
const mineGroup = ref<SelectGroupOption>({
  type: 'group',
  label: t('common.mine'),
  key: 'g_mine',
  children: [] as Array<{ label: string; value: string; is_public: boolean }>,
})
const publicGroup = ref<SelectGroupOption>({
  type: 'group',
  label: t('common.publicAccess'),
  key: 'g_public',
  children: [] as Array<{ label: string; value: string; is_public: boolean }>,
})
const options: Array<SelectOption | SelectGroupOption> = [mineGroup.value, publicGroup.value]

function renderLabel(option: SelectOption): VNodeChild {
  if (option.type === 'group')
    return option.label as string
  return [
    h('div', { class: 'flex items-center' }, {
      default: () => [
        h(
          NIcon,
          {
            style: {
              verticalAlign: '-0.15em',
              marginRight: '4px',
            },
          },
          {
            default: () => h(option.isPublic ? Cloud32Regular : LockClosed32Regular),
          },
        ),
        h(
          'div',
          {
            class: 'ml-1.5',
          },
          { default: () => option.label as string },
        ),
      ],
    }),
  ]
}

function handleSelect(knowledgeBaseUuid: string) {
  let kbName = ''
  let hit = mineGroup.value.children?.find(child => child.value === knowledgeBaseUuid)
  if (!hit)
    hit = publicGroup.value.children?.find(child => child.value === knowledgeBaseUuid)

  if (hit)
    kbName = hit.label as string

  emit('selected', knowledgeBaseUuid, kbName)
}

console.log('selected knowledgeBaseUuid', props.knowledgeBaseUuid)
const handleSearch = debounce(search, 300)
async function search(query: string) {
  try {
    await searchMine(query)
    await searchPublic(query)
  } catch (e) {
    console.log(e)
  }
}

async function searchMine(query: string) {
  const { data } = await api.knowledgeBaseSearchMine<KnowledgeBase.InfoListResp>(query, currentPage.value, pageSize)
  mineGroup.value.children = []
  publicGroup.value.children = []
  data.records.forEach((item) => {
    console.log(item)
    mineGroup.value.children?.push({
      value: item.uuid,
      label: item.title,
      is_public: item.isPublic,
    })
  })
}

async function searchPublic(query: string) {
  const { data: data2 } = await api.knowledgeBaseSearchPublic<KnowledgeBase.InfoListResp>(query, currentPage.value, pageSize)
  if (data2) {
    data2.records.forEach((item) => {
      publicGroup.value.children?.push({
        value: item.uuid,
        label: item.title,
        is_public: item.isPublic,
      })
    })
  }
}

/**
 * 1. 初始化选中项不存在，指定下拉列表的第一个为选中项
 * 2. 初始化选中项存在，检查选中项是否在下拉列表中，没有则请求一次该选中的知识库详情，追回到下拉列表中
 */
async function checkAndGetSelected() {
  if (!selectedKnowledgeUuid.value) {
    if (mineGroup.value.children?.length)
      selectedKnowledgeUuid.value = mineGroup.value.children[0].value as string
    else if (publicGroup.value.children?.length)
      selectedKnowledgeUuid.value = publicGroup.value.children[0].value as string

    if (selectedKnowledgeUuid.value)
      handleSelect(selectedKnowledgeUuid.value)

    return
  }
  let hit = mineGroup.value.children?.find(child => child.value === selectedKnowledgeUuid.value)
  if (!hit)
    hit = publicGroup.value.children?.find(child => child.value === selectedKnowledgeUuid.value)

  if (!hit) {
    const resp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(selectedKnowledgeUuid.value)
    if (resp.data.ownerUuid === userStore.userInfo.uuid) {
      mineGroup.value.children?.push({
        value: resp.data.uuid,
        label: resp.data.title,
        is_public: resp.data.isPublic,
      })
    } else {
      publicGroup.value.children?.push({
        value: resp.data.uuid,
        label: resp.data.title,
        is_public: resp.data.isPublic,
      })
    }
  }
}

watch(
  () => authStore.token,
  () => {
    if (authStore.token) {
      nextTick(async () => {
        await search('')
        await checkAndGetSelected()
      })
    }
  },
  { immediate: true },
)
</script>

<template>
  <NSelect
    ref="selectInstRef" v-model:value="selectedKnowledgeUuid" filterable :placeholder="t('workflow.searchKnowledgeBase')" :options="options"
    clearable remote :render-label="renderLabel" @update:value="handleSelect" @search="handleSearch"
  />
</template>
