<script setup lang='ts'>
import { computed, ref, watch } from 'vue'
import { NButton, NCheckbox, NCheckboxGroup, NFlex, NList, NListItem, NModal, NPopover, NSwitch, NUpload, useMessage } from 'naive-ui'
import type { UploadFileInfo } from 'naive-ui'
import ConvKnowledgeSelector from './ConvKnowledgeSelector.vue'
import { LLMSelector, SvgIcon } from '@/components/common'
import { useAppStore, useAuthStore, useChatStore, useMcpStore } from '@/store'
import { defaultConv } from '@/store/modules/chat/helper'
import { router } from '@/router'
import { t } from '@/locales'
import api from '@/api'
const emit = defineEmits<Emit>()
const allowedImageTypes = ['image/png', 'image/jpeg']
interface Emit {
  (e: 'imagesChange', imageUuids: string[]): void
}
const appStore = useAppStore()
const authStore = useAuthStore()
const chatStore = useChatStore()
const mcpStore = useMcpStore()
const token = ref<string>(authStore.token)
const ms = useMessage()
const uploadedFileInfoList = ref<UploadFileInfo[]>([])
const uploadedUuidList = ref<string[]>([])
const uploadedUrls = ref<string[]>([])
const currConv = computed(() => chatStore.getCurConv || defaultConv())
const canUploadImage = ref<boolean>(false)
const isReasoner = ref<boolean>(false)
const isThinkingClosable = ref<boolean>(false)
const mcpModalShow = ref<boolean>(false)
const knowledgeModalShow = ref<boolean>(false)
const tmpMcpIds = ref<string[]>([])
const tmpConvKbs = ref<Chat.ConvKnowledge[]>([])
const tmpConvKbIds = ref<string[]>([])

async function beforeUpload(data: { file: UploadFileInfo; fileList: UploadFileInfo[] }) {
  const file = data.file.file
  if (!file) {
    ms.error(t('chat.fileNotExist'))
    return false
  }
  if (allowedImageTypes.findIndex(item => item === file.type) === -1) {
    ms.error(t('chat.imageFormatError'))
    return false
  }
  if (file.size > 4 * 1024 * 1024) {
    ms.error(t('chat.fileSizeExceed'))
    return false
  }
  return true
}

function handleFinish({ file, event }: { file: UploadFileInfo; event?: ProgressEvent }) {
  const res = JSON.parse((event?.target as XMLHttpRequest).response)
  if (res.success) {
    uploadedUuidList.value.push(res.data.uuid)
    uploadedUrls.value.push(res.data.url)
    uploadedFileInfoList.value.push(file)
    console.log(`image uuid:${res.data.uuid}`)
  } else {
    console.log(`handleOriginalFinish err:${res.data}`)
  }
  emit('imagesChange', uploadedUrls.value)
}

function handlerRemove({ file }: { file: UploadFileInfo }) {
  const itemIndex = uploadedFileInfoList.value.findIndex(item => item.id === file.id)
  const removeUuid = uploadedUuidList.value.at(itemIndex)
  if (removeUuid) {
    api.fileDel(removeUuid)
    uploadedUrls.value.splice(itemIndex, 1)
    uploadedUuidList.value.splice(itemIndex, 1)
    uploadedFileInfoList.value.splice(itemIndex, 1)
  }
  emit('imagesChange', uploadedUrls.value)
}

// DeepSeek 深度思考模式与工具调用不兼容（langchain4j #3461: partialArguments cannot be null）
// TODO: 升级 langchain4j 后移除此 workaround，恢复工具调用支持
const isDeepSeekThinking = computed(() => {
  const modelName = appStore.selectedLLM?.modelName?.toLowerCase() || ''
  return currConv.value.isEnableThinking && isReasoner.value && modelName.includes('deepseek')
})

function handleMcpModalShow() {
  if (isDeepSeekThinking.value) {
    ms.warning(t('chat.deepThinkingIncompatibleWithTool'))
    return
  }
  mcpModalShow.value = true
  tmpMcpIds.value = [...currConv.value.mcpIds]
}

function handleKnowledgeModalShow() {
  knowledgeModalShow.value = true
  tmpConvKbs.value = [...currConv.value.convKnowledgeList]
  tmpConvKbIds.value = currConv.value.convKnowledgeList.map(kb => kb.id)
}

function handleKnowledgeSave() {
  knowledgeModalShow.value = false
}

async function handleSaveMcps() {
  try {
    currConv.value.mcpIds = tmpMcpIds.value
    await api.convEdit(currConv.value.uuid, { mcpIds: currConv.value.mcpIds })
    chatStore.updateConv(currConv.value.uuid, currConv.value)
  } catch (error) {
    console.error('handleSaveMcps error', error)
  } finally {
    mcpModalShow.value = false
  }
}

function gotoMcp() {
  router.push({ name: 'Mcp' })
  mcpModalShow.value = false
}

function toggleUsingContext() {
  api.convToggleUsingContext(currConv.value.uuid, !currConv.value.understandContextEnable)
  currConv.value.understandContextEnable = !currConv.value.understandContextEnable
  if (currConv.value.understandContextEnable)
    ms.success(t('chat.turnOnContext'))
  else
    ms.warning(t('chat.turnOffContext'))
}

async function toogleThinking() {
  if (!isReasoner.value || !isThinkingClosable.value) {
    console.log('该模型不支持对深度思考功能的开启或关闭')
    return
  }
  currConv.value.isEnableThinking = !currConv.value.isEnableThinking
  await api.convToggleThinking(currConv.value.uuid, currConv.value.isEnableThinking)
  if (currConv.value.isEnableThinking) {
    // DeepSeek 深度思考模式与工具调用不兼容（langchain4j #3461, TODO: 升级后移除）
    if (isDeepSeekThinking.value && currConv.value.mcpIds.length > 0) {
      currConv.value.mcpIds = []
      await api.convEdit(currConv.value.uuid, { mcpIds: [] })
      ms.warning(t('chat.deepThinkingAutoCloseTool'))
    } else {
      ms.success(t('chat.deepThinkingEnabled'))
    }
  } else {
    ms.warning(t('chat.deepThinkingDisabled'))
  }
}

async function toogleWebSearch() {
  if (!appStore.selectedLLM.isSupportWebSearch) {
    console.log('该模型不支持联网搜索功能的开启或关闭')
    return
  }
  if (isDeepSeekThinking.value) {
    ms.warning(t('chat.deepThinkingIncompatibleWithWebSearch'))
    return
  }
  currConv.value.isEnableWebSearch = !currConv.value.isEnableWebSearch
  try {
    await api.convEdit(currConv.value.uuid, { isEnableWebSearch: currConv.value.isEnableWebSearch })
  } catch (err) {
    console.error('toogleWebSearch error', err)
    ms.error(`${t('chat.operationFailed')}${err}`, { duration: 2000 })
    return
  }
  if (currConv.value.isEnableWebSearch)
    ms.success(t('chat.webSearchEnabled'))
  else
    ms.warning(t('chat.webSearchDisabled'))
}

watch(
  () => appStore.selectedLLM,
  (newVal) => {
    isReasoner.value = newVal.isReasoner
    isThinkingClosable.value = newVal.isThinkingClosable
    if (newVal.inputTypes?.includes('image'))
      canUploadImage.value = true
    else
      canUploadImage.value = false
  },
  {
    immediate: true,
  },
)

watch(isDeepSeekThinking, async (newVal) => {
  if (newVal) {
    if (currConv.value.mcpIds.length > 0) {
      currConv.value.mcpIds = []
      await api.convEdit(currConv.value.uuid, { mcpIds: [] })
      ms.warning(t('chat.deepThinkingAutoCloseTool'))
    }
    if (currConv.value.isEnableWebSearch) {
      currConv.value.isEnableWebSearch = false
      try {
        await api.convEdit(currConv.value.uuid, { isEnableWebSearch: false })
      } catch (err) {
        console.error('auto disable webSearch error', err)
      }
      ms.warning(t('chat.deepThinkingAutoCloseWebSearch'))
    }
  }
}, { immediate: true })
</script>

<template>
  <div class="flex flex-col space-x-2 input-tool-bar">
    <div class="flex flex-row space-x-2 items-center py-1.5">
      <div>
        <LLMSelector />
      </div>
      <div
        class="rounded border hover:border-green-600 text-green-600 p-1"
        :class="{ 'cursor-pointer': isReasoner && isThinkingClosable, 'cursor-not-allowed': !isReasoner || !isThinkingClosable }"
        @click="toogleThinking"
      >
        <template v-if="isReasoner && isThinkingClosable">
          {{ t('chat.deepThinking') }}
          <NSwitch :value="currConv.isEnableThinking" size="small" />
        </template>
        <template v-if="isReasoner && !isThinkingClosable">
          <NPopover trigger="hover">
            <template #trigger>
              <div>
                {{ t('chat.deepThinking') }}
                <NSwitch :value="true" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.deepThinkingCannotDisable') }} </span>
          </NPopover>
        </template>
        <template v-if="!isReasoner">
          <NPopover trigger="hover">
            <template #trigger>
              <div>
                {{ t('chat.deepThinking') }}
                <NSwitch :value="false" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.deepThinkingNotSupported') }} </span>
          </NPopover>
        </template>
      </div>
      <div
        class="rounded border hover:border-green-600 text-green-600 p-1"
        :class="{ 'cursor-pointer': appStore.selectedLLM.isSupportWebSearch, 'cursor-not-allowed': !appStore.selectedLLM.isSupportWebSearch }"
        @click="toogleWebSearch"
      >
        <template v-if="appStore.selectedLLM.isSupportWebSearch">
          {{ t('chat.webSearch') }}
          <NSwitch :value="currConv.isEnableWebSearch" size="small" />
        </template>
        <template v-if="!appStore.selectedLLM.isSupportWebSearch">
          <NPopover trigger="hover">
            <template #trigger>
              <div>
                {{ t('chat.webSearch') }}
                <NSwitch :value="false" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.webSearchNotSupported') }} </span>
          </NPopover>
        </template>
      </div>
      <div class="rounded border hover:border-green-600 cursor-pointer p-2" @click="toggleUsingContext">
        <NPopover trigger="hover">
          <template #trigger>
            <span
              :class="{ 'text-[#4b9e5f]': currConv.understandContextEnable, 'text-[#a8071a]': !currConv.understandContextEnable }"
            >
              <SvgIcon icon="ri:chat-history-line" />
            </span>
          </template>
          <span> {{ currConv.understandContextEnable ? t('chat.understandContextEnable')
            : t('chat.understandContextDisable') }} </span>
        </NPopover>
      </div>
      <div class="rounded border hover:border-green-600 hover:text-green-600 cursor-pointer pt-2 px-2">
        <NUpload
          :action="`/api/image/upload?token=${token}`" response-type="text" :disabled="!canUploadImage"
          @before-upload="beforeUpload" @finish="handleFinish"
        >
          <NPopover trigger="hover">
            <template #trigger>
              <span>
                <SvgIcon icon="ri:image-line" />
              </span>
            </template>
            <span> {{ canUploadImage ? t('chat.uploadImageTip') : t('chat.uploadImageNotSupported') }} </span>
          </NPopover>
        </NUpload>
      </div>
      <div
        class="overflow-hidden rounded border hover:border-green-600 p-1 h-8 cursor-pointer"
        @click="handleKnowledgeModalShow"
      >
        <span class="text-xs text-green-600">{{ t('chat.knowledgeBaseLabel') }}</span>
        <template v-for="knolwedge in currConv.convKnowledgeList" :key="knolwedge.uuid">
          <span class="text-xs mr-1">{{ knolwedge.title }}</span>
        </template>
        <span v-if="currConv.convKnowledgeList.length === 0" class="text-xs mr-1">{{ t('common.none') }}</span>
      </div>
      <div class="flex-1 overflow-hidden rounded border hover:border-green-600 cursor-pointer p-1 h-8" @click="handleMcpModalShow">
        <span class="text-xs text-green-600">{{ t('chat.toolLabel') }}</span>
        <template v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid">
          <span v-if="currConv.mcpIds.includes(userMcp.mcpInfo.id)" class="text-xs mr-1">{{ userMcp.mcpInfo.title
          }}</span>
        </template>
        <span v-if="currConv.mcpIds.length === 0" class="text-xs mr-1">{{ t('common.none') }}</span>
      </div>
    </div>
    <NList hoverable show-divider>
      <NListItem v-for="fileInfo in uploadedFileInfoList" :key="fileInfo.id">
        <div class="flex">
          <span class="flex-1 text-xs">{{ fileInfo.name }}</span>
          <SvgIcon
            class="flex-none cursor-pointer text-sm" icon="clarity:remove-line"
            @click="handlerRemove({ file: fileInfo })"
          />
        </div>
      </NListItem>
    </NList>
    <NModal
      v-model:show="knowledgeModalShow" display-directive="show" style="width: 90%; max-width: 800px"
      preset="card" :title="t('chat.configConversationKnowledge')"
    >
      <ConvKnowledgeSelector :tmp-save="false" :conversation="currConv" @submitted="handleKnowledgeSave" />
    </NModal>
    <NModal v-model:show="mcpModalShow" style="width: 90%; max-width: 640px" preset="card" :title="t('chat.configMcp')">
      <NCheckboxGroup v-model:value="tmpMcpIds" class="my-2 flex flex-wrap space-x-2">
        <NCheckbox
          v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid" :value="userMcp.mcpInfo.id"
          :label="userMcp.mcpInfo.title"
        />
      </NCheckboxGroup>
      <span v-if="mcpStore.myUserMcpList.length === 0" class="mr-1">{{ t('common.noData') }}</span>
      <NFlex justify="space-between" class="mt-4">
        <NButton type="primary" text tag="a" class="mt-4" @click="gotoMcp">
          {{ t('chat.goEnableMoreTools') }}
        </NButton>
        <NButton type="primary" @click="handleSaveMcps()">
          {{ t('common.save') }}
        </NButton>
      </NFlex>
    </NModal>
  </div>
</template>

<style lang="less">
.input-tool-bar .n-upload-file-list {
  display: none
}
</style>
