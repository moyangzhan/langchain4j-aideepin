<script setup lang='ts'>
import { computed, onMounted, reactive, ref, watch } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NCard, NEmpty, NSpin, useMessage } from 'naive-ui'
import { useRoute } from 'vue-router'
import DocumentGraphCanvas from './DocumentGraphCanvas.vue'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const route = useRoute()
const ms = useMessage()

const { kbUuid } = route.params as { kbUuid: string; docUuid: string }
const curDocUuid = ref<string>('')

const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const curDoc = reactive<KnowledgeBase.Document>(knowledgeBaseEmptyItem())
const docLoading = ref(false)

const canvasHeight = ref<number>(400)

// Empty state reported by the canvas: null = still loading; once loaded empty, an NEmpty hint replaces the canvas
const graphEmpty = ref<boolean | null>(null)

function onGraphLoaded(isEmpty: boolean) {
  graphEmpty.value = isEmpty
}

const graphicalStatusLabel = computed(() => {
  switch (curDoc.graphicalStatus) {
    case 'DOING':
      return t('knowledgeBase.statusProcessing')
    case 'DONE':
      return t('knowledgeBase.statusGraphitized')
    case 'FAIL':
      return t('knowledgeBase.statusFailed')
    default:
      return t('knowledgeBase.statusPending')
  }
})

async function loadDocInfo(docUuid: string) {
  docLoading.value = true
  try {
    const [kbResp, docResp] = await Promise.all([
      api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid),
      api.knowledgeBaseItemInfo<KnowledgeBase.Document>(docUuid),
    ])
    Object.assign(curKb, kbResp.data)
    Object.assign(curDoc, docResp.data)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    docLoading.value = false
  }
}

watch(
  () => route.params.docUuid,
  (docUuid) => {
    const uuid = Array.isArray(docUuid) ? docUuid[0] : docUuid
    if (uuid) {
      curDocUuid.value = uuid
      graphEmpty.value = null
      loadDocInfo(uuid)
    }
  },
  { immediate: true },
)

onMounted(() => {
  canvasHeight.value = Math.max(400, window.innerHeight - 320)
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
      <NBreadcrumbItem :href="`/#/kb-manage/${kbUuid}/document/${curDocUuid}/detail`">
        {{ curDoc.title }}
      </NBreadcrumbItem>
      <NBreadcrumbItem :clickable="false">
        {{ t('knowledgeBase.graphLabel') }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NCard style="margin-top: 12px" :title="curDoc.title" hoverable>
      <template #header-extra>
        <NSpin v-if="docLoading" :size="14" />
      </template>
      <div class="flex flex-wrap gap-x-6 gap-y-1 mb-3" style="font-size: 12px; opacity: 0.7;">
        <span>{{ t('knowledgeBase.graphLabel') }}: {{ graphicalStatusLabel }}</span>
        <span v-if="curDoc.graphicalStatusChangeTime">{{ t('knowledgeBase.updateTime') }}: {{ curDoc.graphicalStatusChangeTime }}</span>
        <span v-if="curDoc.graphicalStatus === 'FAIL' && curDoc.failReason" style="color: #d03050;">{{ curDoc.failReason }}</span>
      </div>
      <DocumentGraphCanvas v-if="graphEmpty !== true" :doc-uuid="curDocUuid" :height="canvasHeight" @loaded="onGraphLoaded" />
      <div v-else class="flex items-center justify-center" :style="{ height: `${canvasHeight}px` }">
        <NEmpty :description="curDoc.graphicalStatus === 'DOING' ? t('knowledgeBase.graphBuildingTip') : t('knowledgeBase.noGraphData')" />
      </div>
    </NCard>
  </div>
</template>
