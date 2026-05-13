<script setup lang='ts'>
import type { Ref } from 'vue'
import { computed, nextTick, onActivated, onUnmounted, ref, watch } from 'vue'
import { storeToRefs } from 'pinia'
import { NButton, NCollapse, NCollapseItem, NIcon, NInput, NSelect, useDialog, useMessage } from 'naive-ui'
import type { MessageReactive } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { v4 as uuidv4 } from 'uuid'
import { Message } from '../chat/components'
import { useScroll } from '../chat/hooks/useScroll'
import HeaderComponent from '../chat/components/Header/index.vue'
import LoginTip from '@/views/user/LoginTip.vue'
import { HoverButton, LLMSelector, SvgIcon } from '@/components/common'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAiSearchStore, useAppStore, useAuthStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { debounce } from '@/utils/functions/debounce'
import { aiSearchEmptyRecord } from '@/utils/functions'

let controller = new AbortController()
let loadingms: MessageReactive

const ms = useMessage()
const dialog = useDialog()
const appStore = useAppStore()
const aiSearchStore = useAiSearchStore()
const authStore = useAuthStore()
const { loadedAll, nextLoadingMaxId, loadingRecords, sseRequesting, records } = storeToRefs<any>(aiSearchStore)
const { isMobile } = useBasicLayout()
const { scrollRef, scrollToBottom, scrollToBottomIfAtBottom, scrollTo } = useScroll()
const prompt = ref<string>('')
const isBrief = ref<boolean>(false)
const inputRef = ref<Ref | null>(null)

let prevScrollTop: number

function handleChangeEngine(value: string) {
  appStore.setSelectedSearchEngine(value)
}

async function handleSubmit() {
  const message = prompt.value

  if (!message || message.trim() === '')
    return

  if (loadingRecords.value || sseRequesting.value)
    return

  aiSearchStore.setSseRequesting(true)
  controller = new AbortController()

  prompt.value = ''
  scrollToBottom()

  const tmpUuid = uuidv4().replace(/-/g, '')
  const tmpRecord = aiSearchEmptyRecord()
  tmpRecord.uuid = tmpUuid
  tmpRecord.question = message
  tmpRecord.answer = t('aiSearch.generating')
  tmpRecord.loading = true
  tmpRecord.aiModelPlatform = appStore.selectedLLM.modelPlatform

  try {
    aiSearchStore.appendRecord(tmpRecord)

    await api.aiSearchProcess({
      options: {
        searchText: message,
        engineName: appStore.selectedSearchEngine,
        modelName: appStore.selectedLLM.modelName,
        briefSearch: isBrief.value,
      },
      signal: controller.signal,
      startCallback: (chunk) => {
        tmpRecord.answer = ''
        aiSearchStore.updateRecord(tmpRecord)
      },
      messageReceived: (chunk, eventName) => {
        console.log(chunk)

        if (eventName === '[SOURCE_LINKS]') {
          aiSearchStore.setSourceSites(
            tmpUuid,
            JSON.parse(chunk),
          )
        } else {
          // Always process the final line
          aiSearchStore.appendChunk(
            tmpUuid,
            chunk,
          )
        }
        scrollToBottomIfAtBottom()
      },
      thinkingDataReceived: (chunk) => {
        // Handle thinking data if needed
        console.log('Thinking data received:', chunk)
      },
      doneCallback: (chunk) => {
        if (chunk.includes('[META]')) {
          const meta = chunk.replace('[META]', '')
          const metaData: Chat.MetaData = JSON.parse(meta)
          console.info('metaData', metaData)
        } else {
          aiSearchStore.appendChunk(
            tmpUuid,
            chunk,
          )
        }
        tmpRecord.loading = false
        tmpRecord.error = false
        aiSearchStore.updateRecord(tmpRecord)
        aiSearchStore.setSseRequesting(false)
      },
      errorCallback: (error) => {
        ms.warning(`${t('aiSearch.systemTip')}${error}`)
        tmpRecord.answer = `${t('aiSearch.systemTip')}${error}`
        tmpRecord.loading = false
        tmpRecord.error = true
        aiSearchStore.updateRecord(tmpRecord)
        aiSearchStore.setSseRequesting(false)
      },
    })
  } catch (error: any) {
    const errorMessage = error?.message ?? t('common.wrong')
    ms.error(errorMessage)
    tmpRecord.answer = errorMessage
    tmpRecord.error = true
    aiSearchStore.setSseRequesting(false)
  }
}

async function loadMoreMessage(event: any) {
  if (loadedAll.value || loadingRecords.value)
    return

  loadingms = ms.loading(
    t('aiSearch.loading'), {
      duration: 3000,
    })
  try {
    aiSearchStore.setLoadingRecords(true)

    const scrollPosition = event.target.scrollHeight - event.target.scrollTop
    const { data } = await api.aiSearchRecords<AiSearch.AiSearchResp>(nextLoadingMaxId.value, '')
    console.log('kb record response:', data)
    if (data.records) {
      aiSearchStore.setMaxId(data.minId)
      aiSearchStore.appendRecords(data.records)
    }

    nextTick(() => scrollTo(event.target.scrollHeight - scrollPosition))

    if (data.records.length === 0) {
      aiSearchStore.setLoadedAll()
      loadingms.destroy()
      loadingms = ms.warning(t('aiSearch.noMore'), {
        duration: 1000,
      })
    }
  } catch (error) {
    console.error(`loadMoreMessage${error}`)
  } finally {
    aiSearchStore.setLoadingRecords(false)
    loadingms.destroy()
  }
}
const handleLoadMoreMessage = debounce(loadMoreMessage, 300)
async function handleScroll(event: any) {
  const scrollTop = event.target.scrollTop
  if (scrollTop < 50 && (scrollTop < prevScrollTop || prevScrollTop === undefined))
    handleLoadMoreMessage(event)

  prevScrollTop = scrollTop
}

function handleDelete(recordUuid: string) {
  dialog.warning({
    title: t('chat.deleteMessage'),
    content: t('aiSearch.deleteConfirmContent'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: () => {
      api.aiSearchRecordDel(recordUuid)
      aiSearchStore.deleteRecord(recordUuid)
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
  if (sseRequesting.value) {
    controller.abort()
    aiSearchStore.setSseRequesting(false)
  }
}

function toggleBrief() {
  isBrief.value = !isBrief.value
}

const placeholder = computed(() => {
  if (isMobile.value)
    return t('chat.placeholderMobile')
  return t('chat.placeholder')
})

const buttonDisabled = computed(() => {
  return loadingRecords.value || sseRequesting.value || !prompt.value || prompt.value.trim() === ''
})

const footerClass = computed(() => {
  let classes = ['p-4']
  if (isMobile.value)
    classes = ['sticky', 'left-0', 'bottom-0', 'right-0', 'p-2', 'pr-3', 'overflow-hidden']
  return classes
})

async function firstLoad() {
  if (!!records.value && !loadingRecords.value && !sseRequesting.value) {
    try {
      aiSearchStore.setLoadingRecords(true)
      const resp = await api.aiSearchRecords<AiSearch.AiSearchResp>(nextLoadingMaxId.value, '')
      if (resp.data.records) {
        aiSearchStore.setMaxId(resp.data.minId)
        aiSearchStore.appendRecords(resp.data.records)
      }
    } finally {
      aiSearchStore.setLoadingRecords(false)
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
    if (authStore.token)
      firstLoad()
  },
  { immediate: true },
)

onUnmounted(() => {
  if (sseRequesting.value)
    controller.abort()
})

onActivated(async () => {
  scrollToBottom()
})
</script>

<template>
  <div class="chat-box flex flex-col w-full h-full">
    <HeaderComponent v-if="isMobile" :using-context="false" />
    <main class="flex-1 overflow-hidden">
      <div id="scrollRef" ref="scrollRef" class="h-full overflow-hidden overflow-y-auto" @scroll="handleScroll">
        <div
          id="image-wrapper" class="w-full max-w-screen-xl m-auto dark:bg-[#101014]"
          :class="[isMobile ? 'p-2' : 'p-4']"
        >
          <LoginTip v-if="!authStore.token" />
          <template v-else-if="!records.length">
            <div class="flex items-center justify-center mt-4 text-center text-neutral-400">
              <NIcon :component="Cat" size="32" />
              <span class="pl-1">Roar~</span>
            </div>
          </template>

          <template v-else>
            <div v-for="record of records" :key="record.uuid">
              <Message
                :date-time="record.createTime" :text="record.question" :regenerate="false" type="text"
                :inversion="true" :error="record.error" :loading="false" @delete="handleDelete(record.uuid)"
              />
              <Message
                :date-time="record.createTime" :text="!!record.answer ? record.answer : t('aiSearch.noAnswer')"
                :regenerate="false" type="text" :inversion="false" :error="record.error" :loading="record.loading"
                :ai-model-platform="record.aiModelPlatform"
                @delete="handleDelete(record.uuid)"
              >
                <div v-if="record.searchEngineResp.items.length > 0" class="search-quota">
                  <NCollapse>
                    <NCollapseItem :title="t('aiSearch.quote')" name="quote">
                      <ul>
                        <li v-for="searchEngineItem of record.searchEngineResp.items" :key="searchEngineItem.link">
                          <NButton size="tiny" text tag="a" :href="searchEngineItem.link" target="_blank" type="primary">
                            {{ searchEngineItem.title }}
                          </NButton>
                        </li>
                      </ul>
                    </NCollapseItem>
                  </NCollapse>
                </div>
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
          <HoverButton
            v-if="!isMobile"
            :tooltip="isBrief ? t('aiSearch.briefMode') : t('aiSearch.detailMode')"
          >
            <span
              class="text-xl"
              :class="{ 'text-[#4b9e5f]': isBrief, 'text-[#a8071a]': !isBrief }"
              @click="toggleBrief"
            >
              <SvgIcon icon="carbon:folder-details-reference" />
            </span>
          </HoverButton>
          <div class="w-32">
            <NSelect
              :value="appStore.selectedSearchEngine" :options="appStore.searchEngines"
              @update:value="handleChangeEngine"
            />
          </div>
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
  </div>
</template>

<style>
.search-quota .n-collapse-item__header-main{
  font-size: 0.375rem;
}
</style>
