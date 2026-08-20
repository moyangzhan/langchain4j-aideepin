<script setup lang='ts'>
import { computed, onMounted, reactive, ref } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCard, NInput, NSelect, NSpace, NSpin, useMessage } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'
import { knowledgeBaseEmptyInfo, knowledgeBaseEmptyItem } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const ms = useMessage()
const route = useRoute()
const router = useRouter()

const { kbUuid, docUuid } = route.params as { kbUuid: string; docUuid?: string }
const isEdit = computed(() => !!docUuid)

const curKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
const tmpItem = reactive<KnowledgeBase.Item>(knowledgeBaseEmptyItem())
const submitting = ref<boolean>(false)
const loading = ref<boolean>(false)

const segmentModeOptions = [
  { label: t('knowledgeBase.segmentModeText'), value: 'text' },
  { label: t('knowledgeBase.segmentModeQa'), value: 'qa' },
  { label: t('knowledgeBase.segmentModeParentChild'), value: 'parent_child' },
]

const pageTitle = computed(() => {
  return isEdit.value
    ? t('knowledgeBase.knowledgeItemEdit', { title: tmpItem.title })
    : t('knowledgeBase.knowledgeItemAdd')
})

// 控制 input 按钮
const inputStatus = computed(() => tmpItem.title.trim().length < 1 && !submitting.value)

async function saveOrUpdate() {
  try {
    submitting.value = true
    await api.knowledgeBaseItemSaveOrUpdate<KnowledgeBase.Item>(tmpItem)
    ms.success(t('common.saveSuccess'))
    router.back()
  }
  catch (error: any) {
    ms.error(error.message ?? 'error')
  }
  finally {
    submitting.value = false
  }
}

onMounted(async () => {
  loading.value = true
  try {
    const kbResp = await api.knowledgeBaseInfo<KnowledgeBase.Info>(kbUuid)
    Object.assign(curKb, kbResp.data)
    if (isEdit.value && docUuid) {
      const resp = await api.knowledgeBaseItemInfo<KnowledgeBase.Item>(docUuid)
      Object.assign(tmpItem, resp.data)
      if (!tmpItem.segmentMode)
        tmpItem.segmentMode = 'text'
    }
    else {
      tmpItem.kbId = curKb.id
      tmpItem.kbUuid = kbUuid
      tmpItem.segmentMode = 'text'
    }
  }
  finally {
    loading.value = false
  }
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
      <NBreadcrumbItem :clickable="false">
        {{ pageTitle }}
      </NBreadcrumbItem>
    </NBreadcrumb>
    <NCard style="margin-top: 12px" :title="pageTitle" hoverable>
      <NSpin :show="loading">
        <NSpace vertical>
          {{ t('store.title') }}
          <NInput v-model:value="tmpItem.title" maxlength="100" show-count />
          {{ t('knowledgeBase.segmentMode') }}
          <NSelect v-model:value="tmpItem.segmentMode" :options="segmentModeOptions" />
          <div style="font-size: 12px; opacity: 0.7">
            {{ t('knowledgeBase.segmentModeTip') }}
          </div>
          {{ t('knowledgeBase.brief') }}
          <NInput v-model:value="tmpItem.brief" type="textarea" show-count :autosize="{ minRows: 2, maxRows: 3 }" />
          <template v-if="tmpItem.segmentMode !== 'qa'">
            {{ t('common.content') }}
            <NInput
              v-model:value="tmpItem.remark"
              class="content-textarea"
              type="textarea"
              show-count
            />
          </template>
        </NSpace>
      </NSpin>
      <template #footer>
        <div class="flex justify-end gap-2">
          <NButton @click="router.back()">
            {{ t('common.cancel') }}
          </NButton>
          <NButton type="primary" :disabled="inputStatus" :loading="submitting" @click="saveOrUpdate">
            {{ t('common.confirm') }}
          </NButton>
        </div>
      </template>
    </NCard>
  </div>
</template>

<style scoped>
.content-textarea :deep(textarea) {
  height: calc(100vh - 480px) !important;
  resize: none;
}
</style>
