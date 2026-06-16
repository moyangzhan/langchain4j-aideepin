<script setup lang='ts'>
import type { Ref } from 'vue'
import { computed, nextTick, onActivated, ref, watch } from 'vue'
import { NButton, NIcon, NModal, useDialog, useLoadingBar, useMessage } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { useScroll } from '../chat/hooks/useScroll'
import Message from './components/Message/index.vue'
import RuntimeNodes from './components/RuntimeNodes.vue'
import RunDetail from './components/RunDetail.vue'
import LoginTip from '@/views/user/LoginTip.vue'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore, useWfStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { debounce } from '@/utils/functions/debounce'
import { emptyWorkflowInfo } from '@/utils/functions'

interface Props {
  workflow: Workflow.WorkflowInfo
  show: boolean
}
const props = withDefaults(defineProps<Props>(), {
  workflow: () => emptyWorkflowInfo(),
})

const ms = useMessage()
const dialog = useDialog()
const wfStore = useWfStore()
const authStore = useAuthStore()
const loaddingBar = useLoadingBar()
const { isMobile } = useBasicLayout()
const { scrollRef, scrollToBottom, scrollToBottomIfAtBottom, scrollTo } = useScroll()
const currWfUuid = props.workflow.uuid
console.log('instance list currWfUuid', currWfUuid)
const showDetailModal = ref<boolean>(false)
const detailNodes = ref<Workflow.WfRuntimeNode[]>([])
const detailErrorMsg = ref<string>('')
const inputRef = ref<Ref | null>(null)
const loadedAll = ref<boolean>(false)
const pageSize = 20
let currentPage = 1
let prevScrollTop: number

async function loadMoreMessage(callback?: Function) {
  if (wfStore.wfUuidToWfRuntimeLoading.get(currWfUuid) || loadedAll.value)
    return

  loaddingBar.start()
  try {
    wfStore.setLoadingRuntimes(currWfUuid, true)

    const { data } = await api.workflowRuntimes<Workflow.WfRuntimesResp>(currWfUuid, currentPage, pageSize)
    console.log('kb record response:', data)
    if (data.records)
      wfStore.unshiftWfRuntimes(currWfUuid, data.records)

    if (data.records.length < pageSize) {
      loadedAll.value = true
      ms.warning(t('common.noMore'), {
        duration: 3000,
      })
    }
  } catch (error) {
    console.error(`loadMoreMessage${error}`)
  } finally {
    currentPage++
    wfStore.setLoadingRuntimes(currWfUuid, false)
    loaddingBar.finish()

    if (callback)
      callback()
  }
}
const handleLoadMoreMessage = debounce(loadMoreMessage, 300)
async function handleScroll(event: any) {
  const scrollTop = event.target.scrollTop
  const lastScrollClient = event.target.scrollHeight
  if (scrollTop < 50 && (scrollTop < prevScrollTop || prevScrollTop === undefined)) {
    handleLoadMoreMessage(() => {
      nextTick(() => {
        scrollTo(event.target.scrollHeight - lastScrollClient)
      })
    })
  }
  prevScrollTop = scrollTop
}

function handleDelete(instUuid: string) {
  if (wfStore.wfUuidToWfRuntimeLoading.get(currWfUuid))
    return
  dialog.warning({
    title: t('chat.deleteMessage'),
    content: t('workflow.inputAndOutputDeleteConfirm'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: () => {
      api.workflowRuntimeDelete(instUuid)
      wfStore.deleteWfRuntime(currWfUuid, instUuid)
    },
  })
}

const wfRuntimes = computed(() => {
  return wfStore.getWfRuntimes(currWfUuid)
})

const footerClass = computed(() => {
  let classes = ['p-4']
  if (isMobile.value)
    classes = ['sticky', 'left-0', 'bottom-0', 'right-0', 'p-2', 'pr-3', 'overflow-hidden']
  return classes
})

async function firstLoad() {
  if (!currWfUuid || currWfUuid === 'default') {
    console.warn('workflow instance wfuuid is empty or default')
    return
  }
  if (wfRuntimes.value && !wfStore.wfUuidToWfRuntimeLoading.get(currWfUuid) && !wfStore.wfUuidToWfRuntimes.get(currWfUuid)) {
    try {
      wfStore.setLoadingRuntimes(currWfUuid, true)
      const resp = await api.workflowRuntimes<Workflow.WfRuntimesResp>(currWfUuid, currentPage, pageSize)
      nextTick(() => {
        if (resp.data.records) {
          wfStore.setWfRuntimes(currWfUuid, resp.data.records)
          if (resp.data.records.length < pageSize)
            loadedAll.value = true
        }
      })
    } finally {
      wfStore.setLoadingRuntimes(currWfUuid, false)
      currentPage++
    }
    nextTick(() => {
      scrollToBottom()
    })
    if (inputRef.value && !isMobile.value)
      inputRef.value?.focus()
  }
}

function runDone() {
  scrollToBottomIfAtBottom()
}

async function onShowRuntimeDetail(instUuid: string) {
  showDetailModal.value = true
  detailNodes.value = []
  detailErrorMsg.value = ''
  const wfRuntime = wfStore.getWfRuntime(instUuid)
  if (!wfRuntime)
    return
  if (wfRuntime.nodes.length > 0) {
    detailNodes.value = wfRuntime.nodes
    return
  }
  try {
    const { data: nodes } = await api.workflowRuntimeNodes<Workflow.WfRuntimeNode[]>(instUuid)
    wfStore.setWfRuntimeNodes(instUuid, nodes)
  } catch (error) {
    console.error('onShowRuntimeDetail error', error)
  } finally {
    const runtime = wfStore.getWfRuntime(instUuid)
    if (runtime) {
      detailNodes.value = runtime.nodes || []
      detailErrorMsg.value = runtime.status === 4 ? runtime.statusRemark : ''
    }
  }
}

function runError(errorMsg: string) {
}

watch(
  () => authStore.token,
  () => {
    if (authStore.token)
      firstLoad()
  },
  { immediate: true },
)

onActivated(async () => {
  scrollToBottom()
})
</script>

<template>
  <main v-show="show" class="flex-1 overflow-hidden">
    <div ref="scrollRef" class="h-full overflow-hidden overflow-y-auto" @scroll="handleScroll">
      <div class="w-full max-w-screen-xl m-auto dark:bg-[#101014]" :class="[isMobile ? 'p-2' : 'p-4']">
        <LoginTip v-if="!authStore.token" />
        <template v-else-if="!wfRuntimes.length">
          <div class="flex items-center justify-center mt-4 text-center text-neutral-400">
            <NIcon :component="Cat" size="32" />
            <span class="pl-1">Roar~</span>
          </div>
        </template>
        <template v-else>
          <div v-for="wfRuntime of wfRuntimes" :key="wfRuntime.uuid">
            <Message
              :wf-runtime="wfRuntime" :io-object="wfRuntime.input" :inversion="true" :loading="false"
              @delete="handleDelete(wfRuntime.uuid)"
            />
            <Message
              :workflow="workflow" :wf-runtime="wfRuntime" :io-object="wfRuntime.output"
              :error-msg="wfRuntime.statusRemark" :inversion="false" :loading="wfRuntime.loading"
              :input-tokens="wfRuntime.inputTokens" :output-tokens="wfRuntime.outputTokens"
              :duration="wfRuntime.duration"
            >
              <NButton v-if="!wfRuntime.loading" size="tiny" text @click="onShowRuntimeDetail(wfRuntime.uuid)">
                {{ t('workflow.runDetail') }}
              </NButton>
            </Message>
          </div>
        </template>
      </div>
    </div>
  </main>
  <footer v-show="show" :class="footerClass">
    <RunDetail :workflow="workflow" @run-done="runDone" @run-error="runError" />
  </footer>
  <NModal v-model:show="showDetailModal" :title="t('workflow.runDetail')" style="width: 90%; max-width: 900px;" preset="card">
    <RuntimeNodes :workflow="workflow" :nodes="detailNodes" :error-msg="detailErrorMsg" class="max-h-[750px] overflow-y-auto" />
  </NModal>
</template>
