<script setup lang='ts'>
import { nextTick, onMounted, ref, watch } from 'vue'
import { storeToRefs } from 'pinia'
import { NButton, NTabPane, NTabs } from 'naive-ui'
import SubList from './SubList.vue'
import { useAuthStore, useWfStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
const currentPage = ref<number>(1)
const pageSize = 20
const wfStore = useWfStore()
const { activeUuid, myWorkflows, publicWorkflows, selectedType } = storeToRefs<any>(wfStore)
const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const innerHeight = window.innerHeight < 800 ? 800 : window.innerHeight - 60

async function initAll() {
  await loadWfComponents()
  await initMyList()
  await initPublicList()
}

async function initWhenNotLogin() {
  await loadWfComponents()
  await initPublicList()
}

async function loadWfComponents() {
  if (wfStore.wfComponents.length > 0)
    return

  console.log('load wf components')
  const { data: components } = await api.workflowComponents<Workflow.WorkflowComponent[]>()
  if (components && components.length > 0)
    wfStore.setWorkflowComponents(components)
  else
    console.log('workflow components is null')
}

async function initMyList() {
  if (wfStore.loadingMyWorkflows || wfStore.myWorkflows.length > 0)
    return
  console.log('load my workflows')
  wfStore.setLoadingMyWorkflows(true)
  try {
    const { data } = await api.workflowSearchMine<Workflow.InfoListResp>('', currentPage.value, pageSize)
    console.log('loaded my workflows')
    if (data.records) {
      nextTick(() => {
        wfStore.appendWorkflows(data.records, true)
        // 首次打开工作流时
        if (activeUuid.value === 'default') {
          console.log('首次打开工作流')
          wfStore.setActiveAndGo(myWorkflows.value[0].uuid)
        }
      })
    }
  } catch (e) {
    console.error(e)
  } finally {
    wfStore.setLoadingMyWorkflows(false)
  }
}

async function initPublicList() {
  if (wfStore.loadingPublicWorkflows || wfStore.publicWorkflows.length > 0)
    return
  wfStore.setLoadingPublicWorkflows(true)
  try {
    const { data: publicData } = await api.workflowSearchPublic<Workflow.InfoListResp>('', currentPage.value, pageSize)
    if (publicData.records)
      wfStore.appendWorkflows(publicData.records, false)
  } catch (e) {
    console.error(e)
  } finally {
    wfStore.setLoadingPublicWorkflows(false)
  }
}

function handleAdd(this: any) {
  wfStore.setShowCreateView(true, '')
}

watch(
  () => authStoreRef.value.token,
  (newVal) => {
    if (newVal)
      initMyList()
  },
)

onMounted(() => {
  console.log('workflow list onMounted')
  if (authStoreRef.value.token)
    initAll()
  else
    initWhenNotLogin()
})
</script>

<template>
  <NTabs v-model:value="selectedType" tab-class="h-10" pane-class="h-full" type="line" justify-content="space-evenly">
    <NTabPane name="mine" :tab="t('common.mine')" size="small">
      <div class="flex flex-col space-y-2" :style="`height:${innerHeight}px`">
        <div class="px-4">
          <NButton dashed block @click="handleAdd">
            {{ t('workflow.newApp') }}
          </NButton>
        </div>
        <SubList :list="myWorkflows" :active-wf-uuid="activeUuid" />
      </div>
    </NTabPane>
    <NTabPane name="public" :tab="t('common.public')">
      <SubList :list="publicWorkflows" :active-wf-uuid="activeUuid" />
    </NTabPane>
  </NTabs>
</template>
