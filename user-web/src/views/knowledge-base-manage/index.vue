<script setup lang='ts'>
import type { DataTableColumns } from 'naive-ui'
import { computed, h, reactive, ref, watch } from 'vue'
import { NBreadcrumb, NBreadcrumbItem, NButton, NCollapse, NCollapseItem, NDataTable, NIcon, NInput, NInputNumber, NModal, NRadio, NRadioGroup, NSelect, NTooltip, useDialog, useMessage } from 'naive-ui'
import { RouterLink, useRouter } from 'vue-router'
import { QuestionCircle16Regular } from '@vicons/fluent'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAppStore, useAuthStore, useKbStore } from '@/store'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import { TOKEN_ESTIMATOR } from '@/utils/constant'
import { t } from '@/locales'
import api from '@/api'

const router = useRouter()
const dialog = useDialog()
const ms = useMessage()
const appStore = useAppStore()
const loading = ref(false)
const submitting = ref(false)
const showModal = ref(false)
const infoList = ref<KnowledgeBase.Info[]>([])
const paginationReactive = reactive({
  page: 1,
  pageSize: 20,
  itemCount: 0,
})
const searchValue = ref<string>('')
const tmpKb = reactive<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())
// 控制 input 按钮
const inputStatus = computed(() => tmpKb.title.trim().length < 1 && !submitting.value)
const { isMobile } = useBasicLayout()
const authStore = useAuthStore()
const kbStore = useKbStore()
const token = ref<string>(authStore.token)
const itemBoxClass = 'bg-gray-100 rounded-md px-2 pt-1 pb-2 space-y-1'

const changeShowModal = (selected: KnowledgeBase.Info = knowledgeBaseEmptyInfo()) => {
  Object.assign(tmpKb, selected)
  showModal.value = !showModal.value
  if (!tmpKb.ingestModelName) {
    const firstEnableModel = appStore.llms.find((item: { enable: any }) => item.enable)
    if (firstEnableModel) {
      tmpKb.ingestModelName = firstEnableModel.modelName
      tmpKb.ingestModelId = firstEnableModel.modelId
    }
  } else {
    tmpKb.ingestModelName = appStore.llms.find(item => item.modelName === tmpKb.ingestModelName)?.modelName || ''
  }
  if (!tmpKb.ingestTokenEstimator)
    tmpKb.ingestTokenEstimator = TOKEN_ESTIMATOR[0].value
}
// table相关
const createColumns = (): DataTableColumns<KnowledgeBase.Info> => {
  return [
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
      width: 120,
      align: 'center',
      render(row) {
        return h('div', { class: 'grid gap-1' }, {
          default: () => [
            h(
              NButton,
              {
                tertiary: true,
                size: 'tiny',
                type: 'info',
                class: 'col-span-2',
                onClick: () => router.push({ name: 'KnowledgeBaseManageDetail', params: { kbUuid: row.uuid } }),
              },
              { default: () => t('knowledgeBase.enterKnowledgeBase') },
            ),
            h(
              NButton,
              {
                tertiary: true,
                size: 'tiny',
                type: 'info',
                onClick: () => changeShowModal(row),
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
          ],
        })
      },
    },
  ]
}

const columns = createColumns()

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

async function saveOrUpdateKb() {
  try {
    submitting.value = true
    const res = await api.knowledgeBaseSaveOrUpdate<KnowledgeBase.Info>(tmpKb)
    if (tmpKb.id && tmpKb.id !== '0') {
      const hit = infoList.value.find(item => item.id === tmpKb.id)
      if (hit)
        Object.assign(hit, res.data)
    } else {
      infoList.value.push(res.data)
      kbStore.appendMyNewKbInfo(res.data)
    }
    Object.assign(tmpKb, res.data)

    kbStore.setReloadKbInfosSignal(true)
    search(1)
  } finally {
    submitting.value = false
    showModal.value = false
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

function onModelChange(modelName: string) {
  tmpKb.ingestModelName = modelName
  tmpKb.ingestModelId = appStore.llms.find(item => item.modelName === modelName)?.modelId || ''
}

function onTokenEstimatorChange(tokenEstimator: string) {
  tmpKb.ingestTokenEstimator = tokenEstimator
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
        <NButton type="primary" size="small" @click="changeShowModal()">
          {{ $t('common.add') }}
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
      remote :loading="loading" :columns="columns" :data="infoList" :pagination="paginationReactive"
      :single-line="false" :bordered="true" @update:page="onHandlePageChange"
    />
  </div>

  <NModal
    v-model:show="showModal" :title="tmpKb.id === '0' ? t('common.newCreate') : t('common.edit')" style="width: 90%; max-width: 700px; "
    preset="card"
  >
    <div class="max-h-[600px] overflow-y-auto pr-2">
      <div class="flex flex-col space-y-2">
        <div :class="itemBoxClass">
          <div>{{ t('common.title') }}<span class="text-red-400"> *</span></div>
          <NInput v-model:value="tmpKb.title" maxlength="100" :placeholder="t('store.title')" show-count />
        </div>
        <div :class="itemBoxClass">
          <div>{{ t('common.description') }}</div>
          <NInput
            v-model:value="tmpKb.remark" type="textarea" :placeholder="t('store.description')" maxlength="500"
            show-count :autosize="{ minRows: 3, maxRows: 10 }"
          />
        </div>
        <div :class="itemBoxClass">
          <div>{{ t('knowledgeBase.isPublic') }}</div>
          <NRadioGroup v-model:value="tmpKb.isPublic" name="radiogroup">
            <NRadio key="public_yes" :value="true">
              {{ t('common.public') }}
            </NRadio>
            <NRadio key="public_no" :value="false">
              {{ t('common.private') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <div :class="itemBoxClass">
          <div>
            {{ t('knowledgeBase.strictMode') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="padding-top: 0.1rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <div>{{ t('knowledgeBase.strictModeDescShort') }}</div>
              <div>{{ t('knowledgeBase.looseModeDescShort') }}</div>
            </NTooltip>
          </div>
          <NRadioGroup v-model:value="tmpKb.isStrict" name="radiogroup">
            <NRadio key="strict_yes" :value="true">
              {{ t('common.yes') }}
            </NRadio>
            <NRadio key="strict_no" :value="false">
              {{ t('common.no') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <NCollapse>
          <NCollapseItem :title="t('knowledgeBase.docIndexSettingVector')">
            <div class="flex flex-col space-y-2">
              <div :class="itemBoxClass">
                <div>{{ t('knowledgeBase.docOverlapCount') }}</div>
                <NInputNumber v-model:value="tmpKb.ingestMaxOverlap" />
              </div>
              <div :class="itemBoxClass">
                <div>
                  {{ t('knowledgeBase.tokenCounter') }}
                </div>
                <NSelect
                  :value="tmpKb.ingestTokenEstimator" :options="TOKEN_ESTIMATOR"
                  :on-update:value="onTokenEstimatorChange"
                />
              </div>
            </div>
          </NCollapseItem>
          <NCollapseItem :title="t('knowledgeBase.docIndexSettingGraph')">
            <div class="flex flex-col space-y-2">
              <div :class="itemBoxClass">
                <div>
                  {{ t('knowledgeBase.modelName') }}
                  <NTooltip trigger="hover">
                    <template #trigger>
                      <NIcon style="padding-top: 0.1rem">
                        <QuestionCircle16Regular />
                      </NIcon>
                    </template>
                    <div>{{ t('knowledgeBase.modelExtractTip') }}</div>
                  </NTooltip>
                </div>
                <NSelect :value="tmpKb.ingestModelName" :options="appStore.llms" :on-update:value="onModelChange" />
              </div>
            </div>
          </NCollapseItem>
          <NCollapseItem :title="t('knowledgeBase.docRecallSetting')">
            <div class="flex flex-col space-y-2">
              <div :class="itemBoxClass">
                <div>{{ t('knowledgeBase.docRecallMaxCount') }}</div>
                <NInputNumber v-model:value="tmpKb.retrieveMaxResults" />
              </div>
              <div :class="itemBoxClass">
                <div>{{ t('knowledgeBase.docRecallMinScore') }}</div>
                <NInputNumber v-model:value="tmpKb.retrieveMinScore" :precision="1" :min="0" :max="1" />
              </div>
            </div>
          </NCollapseItem>
          <NCollapseItem :title="t('knowledgeBase.llmParamSetting')">
            <div class="flex flex-col space-y-2">
              <div :class="itemBoxClass">
                <div>{{ t('knowledgeBase.systemPromptRole') }}</div>
                <NInput
                  v-model:value="tmpKb.querySystemMessage" type="textarea"
                  :autosize="{ minRows: 2, maxRows: 5 }"
                />
              </div>
              <div :class="itemBoxClass">
                <div>{{ t('knowledgeBase.responseCreativity') }}</div>
                <NInputNumber v-model:value="tmpKb.queryLlmTemperature" :precision="1" :min="0" :max="1" />
              </div>
            </div>
          </NCollapseItem>
        </NCollapse>
      </div>
    </div>
    <template #footer>
      <div class="flex space-x-2 justify-end">
        <NButton type="primary" size="small" :disabled="inputStatus" @click="() => { saveOrUpdateKb() }">
          {{ t('common.confirm') }}
        </NButton>
        <NButton size="small" :disabled="inputStatus" @click="() => { showModal = false }">
          {{ t('common.cancel') }}
        </NButton>
      </div>
    </template>
  </NModal>
</template>
