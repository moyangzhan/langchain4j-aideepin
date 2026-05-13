<script setup lang='ts'>
import type { Ref } from 'vue'
import { computed, nextTick, onActivated, onMounted, onUnmounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { NButton, NCollapse, NCollapseItem, NFlex, NIcon, NInput, NModal, useDialog, useLoadingBar, useMessage } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { Message } from '../chat/components'
import { useScroll } from '../chat/hooks/useScroll'
import HeaderComponent from './Header/index.vue'
import PCHeader from './Header/pc.vue'
import RefGraph from './RefGraph.vue'
import LoginTip from '@/views/user/LoginTip.vue'
import { LLMSelector, SvgIcon } from '@/components/common'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAppStore, useAuthStore, useKbStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { debounce } from '@/utils/functions/debounce'

let controller = new AbortController()

const route = useRoute()
const ms = useMessage()
const dialog = useDialog()
const appStore = useAppStore()
const kbStore = useKbStore()
const authStore = useAuthStore()
const loaddingBar = useLoadingBar()
const { isMobile } = useBasicLayout()
const { scrollRef, scrollToBottom, scrollToBottomIfAtBottom, scrollTo } = useScroll()
const { kbUuid: currKbUuid } = route.params as { kbUuid: string }
console.log('currKbUUid', currKbUuid)
const showReferenceModal = ref<boolean>(false)
const showReferenceRecordUuid = ref<string>('')
const references = ref<KnowledgeBase.QaRecordEmbeddingRef[]>([])
const showRefGraphModal = ref<boolean>(false)
const showRefGraphRecordUuid = ref<string>('')
const prompt = ref<string>('')
const inputRef = ref<Ref | null>(null)

const loadedAll = ref<boolean>(false)
const sseRequesting = ref<boolean>(false)
const pageSize = 20
let currentPage = 1
let prevScrollTop: number

async function handleSubmit() {
  if (!authStore.checkLoginOrShow())
    return

  const message = prompt.value

  if (!message || message.trim() === '')
    return

  if (sseRequesting.value)
    return

  sseRequesting.value = true
  controller = new AbortController()

  prompt.value = ''

  const { data: qaRecord } = await api.knowledgeBaseQaRecordAdd<KnowledgeBase.QaRecordInfo>(currKbUuid, { question: message, modelName: appStore.selectedLLM.modelName })
  qaRecord.answer = t('common.generating')
  qaRecord.loading = true
  qaRecord.aiModelPlatform = appStore.selectedLLM.modelPlatform

  nextTick(() => {
    scrollToBottom()
  })

  try {
    kbStore.appendRecord(currKbUuid, qaRecord)

    await api.knowledgeBaseQaSseAsk({
      options: {
        qaRecordUuid: qaRecord.uuid,
      },
      signal: controller.signal,
      startCallback: () => {
        qaRecord.answer = ''
        kbStore.updateRecord(currKbUuid, qaRecord.uuid, qaRecord)
      },
      thinkingDataReceived: (chunk) => {
        // 处理思考数据
        console.log('Thinking data received:', chunk)
      },
      messageReceived: (chunk) => {
        try {
          kbStore.appendChunk(
            currKbUuid,
            qaRecord.uuid,
            chunk,
          )
        } catch (error) {
          console.error(error)
        }
        scrollToBottomIfAtBottom()
      },
      doneCallback: (chunk) => {
        if (chunk.includes('[META]')) {
          const meta = chunk.replace('[META]', '')
          const metaData: Chat.MetaData = JSON.parse(meta)
          console.info('metaData', metaData)
        } else {
          kbStore.appendChunk(
            currKbUuid,
            qaRecord.uuid,
            chunk,
          )
        }
        qaRecord.loading = false
        qaRecord.error = false
        kbStore.updateRecord(currKbUuid, qaRecord.uuid, qaRecord)
        sseRequesting.value = false
      },
      errorCallback: (error) => {
        sseRequesting.value = false
        ms.warning(`${t('common.systemTip')}${error}`)
        qaRecord.answer = `${t('common.systemTip')}${error}`
        qaRecord.loading = false
        qaRecord.error = true
        kbStore.updateRecord(currKbUuid, qaRecord.uuid, qaRecord)
      },
    })
  } catch (error: any) {
    const errorMessage = error?.message ?? t('common.wrong')
    ms.error(errorMessage)
    qaRecord.answer = errorMessage
    qaRecord.error = true
    sseRequesting.value = false
  }
}

async function loadMoreMessage(callback?: Function) {
  if (kbStore.loadingRecords.get(currKbUuid) || loadedAll.value)
    return

  loaddingBar.start()
  try {
    kbStore.setLoadingRecords(currKbUuid, true)

    const { data } = await api.knowledgeBaseQaRecordSearch<KnowledgeBase.QaRecordListResp>(currKbUuid, '', currentPage, pageSize)
    console.log('kb record response:', data)
    if (data.records)
      kbStore.appendRecords(currKbUuid, data.records)

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
    kbStore.setLoadingRecords(currKbUuid, false)
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

function handleDelete(qaRecordUuid: string) {
  if (kbStore.loadingRecords.get(currKbUuid))
    return
  dialog.warning({
    title: t('chat.deleteMessage'),
    content: t('knowledgeBase.questionAndAnswer'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: () => {
      api.knowledgeBaseQaRecordDel(qaRecordUuid)
      kbStore.deleteRecord(currKbUuid, qaRecordUuid)
    },
  })
}

function handleEnter(event: KeyboardEvent) {
  if (!isMobile.value) {
    if (event.key === 'Enter' && !event.shiftKey) {
      event.preventDefault()
      handleSubmit()
    }
  } else {
    if (event.key === 'Enter' && event.ctrlKey) {
      event.preventDefault()
      handleSubmit()
    }
  }
}

function handleStop() {
  if (kbStore.loadingRecords.get(currKbUuid))
    controller.abort()

  sseRequesting.value = false
}

// 打开引用
async function handleReferenceClick(qaRecordUuid: string) {
  showReferenceModal.value = true
  showReferenceRecordUuid.value = qaRecordUuid
  references.value = []
  references.value = kbStore.getReferences(qaRecordUuid)
  if (references.value.length === 0) {
    const { data } = await api.knowledgeBaseEmbeddingRef(qaRecordUuid)
    kbStore.setQaRecordReferences(qaRecordUuid, data)

    // 显示最后一次点击的引用
    references.value = kbStore.getReferences(showReferenceRecordUuid.value)
  }
}

async function handleGraphClick(qaRecordUuid: string) {
  showRefGraphModal.value = true
  showRefGraphRecordUuid.value = qaRecordUuid
}

const qaRecords = computed(() => {
  console.log('qaRecords computed')
  return kbStore.getRecords(currKbUuid)
})

const placeholder = computed(() => {
  if (isMobile.value)
    return t('chat.placeholderMobile')
  return t('chat.placeholder')
})

const buttonDisabled = computed(() => {
  return sseRequesting.value || !prompt.value || prompt.value.trim() === ''
})

const footerClass = computed(() => {
  let classes = ['p-4']
  if (isMobile.value)
    classes = ['sticky', 'left-0', 'bottom-0', 'right-0', 'p-2', 'pr-3', 'overflow-hidden']
  return classes
})

async function firstLoad() {
  if (!!qaRecords.value && !kbStore.loadingRecords.get(currKbUuid) && !kbStore.kbUuidToQaRecords.get(currKbUuid)) {
    try {
      kbStore.setLoadingRecords(currKbUuid, true)
      const resp = await api.knowledgeBaseQaRecordSearch<KnowledgeBase.QaRecordListResp>(currKbUuid, '', currentPage, pageSize)
      if (resp.data.records) {
        kbStore.appendRecords(currKbUuid, resp.data.records)
        if (resp.data.records.length < pageSize)
          loadedAll.value = true
      }
    } finally {
      kbStore.setLoadingRecords(currKbUuid, false)
      currentPage++
    }
    nextTick(() => {
      scrollToBottom()
    })
    if (inputRef.value && !isMobile.value)
      inputRef.value?.focus()
  }
}

watch(
  () => authStore.token,
  () => {
    if (authStore.token) {
      console.log('kb first load')
      firstLoad()
    }
  },
  { immediate: true },
)

onMounted(async () => {
  console.log('knowledge-base onmounted', currKbUuid)
})

onUnmounted(() => {
  if (kbStore.loadingRecords.get(currKbUuid))
    controller.abort()
})

onActivated(async () => {
  scrollToBottom()
})
</script>

<template>
  <div class="chat-box flex flex-col w-full h-full">
    <HeaderComponent v-if="isMobile" :using-context="false" />
    <PCHeader v-else :knowledge-base="kbStore.getSelectedKb as KnowledgeBase.Info" />
    <main class="flex-1 overflow-hidden">
      <div id="scrollRef" ref="scrollRef" class="h-full overflow-hidden overflow-y-auto" @scroll="handleScroll">
        <div
          id="image-wrapper" class="w-full max-w-screen-xl m-auto dark:bg-[#101014]"
          :class="[isMobile ? 'p-2' : 'p-4']"
        >
          <LoginTip v-if="!authStore.token" />
          <template v-else-if="!qaRecords.length">
            <div class="flex items-center justify-center mt-4 text-center text-neutral-400">
              <NIcon :component="Cat" size="32" />
              <span class="pl-1">Roar~</span>
            </div>
          </template>

          <template v-else>
            <div v-for="qaRecord of qaRecords" :key="qaRecord.uuid">
              <Message
                :date-time="qaRecord.createTime" :text="qaRecord.question" :regenerate="false" type="text"
                :inversion="true" :error="qaRecord.error" :loading="false" @delete="handleDelete(qaRecord.uuid)"
              />
              <Message
                :date-time="qaRecord.createTime" :text="!!qaRecord.answer ? qaRecord.answer : t('aiSearch.noAnswer')"
                :regenerate="false" type="text" :inversion="false" :error="qaRecord.error" :loading="qaRecord.loading"
                :ai-model-platform="qaRecord.aiModelPlatform" @delete="handleDelete(qaRecord.uuid)"
              >
                <NFlex>
                  <NButton
                    v-if="!!qaRecord.answer && !qaRecord.loading" size="tiny" text type="primary"
                    @click="handleReferenceClick(qaRecord.uuid)"
                  >
                    {{ t('chat.reference') }}
                  </NButton>

                  <NButton
                    v-if="!!qaRecord.answer && !qaRecord.loading" size="tiny" text type="primary"
                    @click="handleGraphClick(qaRecord.uuid)"
                  >
                    {{ t('chat.graph') }}
                  </NButton>
                </NFlex>
              </Message>
            </div>
          </template>
        </div>
      </div>
      <div class="sticky bottom-0 left-0 flex justify-center">
        <NButton v-if="sseRequesting" size="tiny" @click="handleStop">
          <template #icon>
            <SvgIcon icon="ri:stop-circle-line" />
          </template>
          {{ t('common.stopRequest') }}
        </NButton>
      </div>
    </main>
    <footer :class="footerClass">
      <div class="w-full max-w-screen-xl m-auto">
        <div class="flex items-center justify-between space-x-2">
          <div class="w-48">
            <LLMSelector />
          </div>
          <NInput
            ref="inputRef" v-model:value="prompt" type="textarea" :placeholder="placeholder"
            :autosize="{ minRows: 1, maxRows: isMobile ? 4 : 8 }" @keypress="handleEnter"
          />
          <NButton type="primary" :disabled="buttonDisabled" @click="handleSubmit">
            <template #icon>
              <span class="dark:text-black">
                <SvgIcon icon="ri:send-plane-fill" />
              </span>
            </template>
          </NButton>
        </div>
      </div>
    </footer>

    <NModal v-model:show="showReferenceModal" style="max-width: 80%;" preset="card" :title="t('chat.referenceMaterial')">
      <div v-show="references.length === 0">
        {{ t('common.none') }}
      </div>
      <NCollapse v-show="references.length > 0" :default-expanded-names="['refer_0']">
        <NCollapseItem
          v-for="(reference, idx) of references" :key="reference.embeddingId" :title="`${t('aiSearch.quote')}${idx + 1}`"
          :name="`refer_${idx}`"
        >
          {{ reference.text }}
        </NCollapseItem>
      </NCollapse>
    </NModal>

    <NModal v-model:show="showRefGraphModal" display-directive="show" style="max-width: 80%;" preset="card" :title="t('chat.referenceGraph')">
      <RefGraph :qa-record-uuid="showRefGraphRecordUuid" />
    </NModal>
  </div>
</template>
