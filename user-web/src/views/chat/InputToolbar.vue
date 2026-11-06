<script setup lang='ts'>
import { computed, ref, watch } from 'vue'
import { NButton, NCheckbox, NCheckboxGroup, NFlex, NModal, NPopover, NSwitch, NUpload, useMessage } from 'naive-ui'
import type { UploadFileInfo } from 'naive-ui'
import ConvKnowledgeSelector from './ConvKnowledgeSelector.vue'
import { LLMSelector, SvgIcon } from '@/components/common'
import { useAppStore, useAuthStore, useChatStore, useMcpStore } from '@/store'
import { getDefaultCharacter } from '@/store/modules/chat/helper'
import { router } from '@/router'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { t } from '@/locales'
import api from '@/api'
const emit = defineEmits<Emit>()
const allowedImageTypes = ['image/png', 'image/jpeg']
interface Emit {
  (e: 'uploadFinish', payload: { file: UploadFileInfo; uuid: string; url: string }): void
}
const appStore = useAppStore()
const authStore = useAuthStore()
const chatStore = useChatStore()
const mcpStore = useMcpStore()
const token = ref<string>(authStore.token)
const ms = useMessage()
const { isMobile } = useBasicLayout()
const currCharacter = computed(() => chatStore.getCurCharacter || getDefaultCharacter())
const canUploadImage = ref<boolean>(false)
const isReasoner = ref<boolean>(false)
const isThinkingClosable = ref<boolean>(false)
const mcpModalShow = ref<boolean>(false)
const knowledgeModalShow = ref<boolean>(false)
const tmpMcpIds = ref<string[]>([])
const tmpCharacterKbs = ref<Chat.CharacterKnowledge[]>([])
const tmpCharacterKbIds = ref<string[]>([])

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
  if (res.success)
    emit('uploadFinish', { file, uuid: res.data.uuid, url: res.data.url })
  else
    console.log(`handleFinish err:${res.data}`)
}

// DeepSeek 深度思考模式与工具调用不兼容（langchain4j #3461: partialArguments cannot be null）
// TODO: 升级 langchain4j 后移除此 workaround，恢复工具调用支持
const isDeepSeekThinking = computed(() => {
  const modelName = appStore.selectedLLM?.modelName?.toLowerCase() || ''
  return currCharacter.value.isEnableThinking && isReasoner.value && modelName.includes('deepseek')
})

function handleMcpModalShow() {
  if (isDeepSeekThinking.value) {
    ms.warning(t('chat.deepThinkingIncompatibleWithTool'))
    return
  }
  mcpModalShow.value = true
  tmpMcpIds.value = [...currCharacter.value.mcpIds]
}

function handleKnowledgeModalShow() {
  knowledgeModalShow.value = true
  tmpCharacterKbs.value = [...currCharacter.value.characterKnowledgeList]
  tmpCharacterKbIds.value = currCharacter.value.characterKnowledgeList.map(kb => kb.id)
}

function handleKnowledgeSave() {
  knowledgeModalShow.value = false
}

async function handleSaveMcps() {
  try {
    currCharacter.value.mcpIds = tmpMcpIds.value
    await api.characterEdit(currCharacter.value.uuid, { mcpIds: currCharacter.value.mcpIds })
    chatStore.updateCharacter(currCharacter.value.uuid, currCharacter.value)
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
  api.characterToggleUsingContext(currCharacter.value.uuid, !currCharacter.value.understandContextEnable)
  currCharacter.value.understandContextEnable = !currCharacter.value.understandContextEnable
  if (currCharacter.value.understandContextEnable)
    ms.success(t('chat.turnOnContext'))
  else
    ms.warning(t('chat.turnOffContext'))
}

async function toogleThinking() {
  if (!isReasoner.value || !isThinkingClosable.value) {
    console.log('该模型不支持对深度思考功能的开启或关闭')
    return
  }
  currCharacter.value.isEnableThinking = !currCharacter.value.isEnableThinking
  await api.characterToggleThinking(currCharacter.value.uuid, currCharacter.value.isEnableThinking)
  if (currCharacter.value.isEnableThinking) {
    // DeepSeek 深度思考模式与工具调用不兼容（langchain4j #3461, TODO: 升级后移除）
    if (isDeepSeekThinking.value && currCharacter.value.mcpIds.length > 0) {
      currCharacter.value.mcpIds = []
      await api.characterEdit(currCharacter.value.uuid, { mcpIds: [] })
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
  currCharacter.value.isEnableWebSearch = !currCharacter.value.isEnableWebSearch
  try {
    await api.characterEdit(currCharacter.value.uuid, { isEnableWebSearch: currCharacter.value.isEnableWebSearch })
  } catch (err) {
    console.error('toogleWebSearch error', err)
    ms.error(`${t('chat.operationFailed')}${err}`, { duration: 2000 })
    return
  }
  if (currCharacter.value.isEnableWebSearch)
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
    if (currCharacter.value.mcpIds.length > 0) {
      currCharacter.value.mcpIds = []
      await api.characterEdit(currCharacter.value.uuid, { mcpIds: [] })
      ms.warning(t('chat.deepThinkingAutoCloseTool'))
    }
    if (currCharacter.value.isEnableWebSearch) {
      currCharacter.value.isEnableWebSearch = false
      try {
        await api.characterEdit(currCharacter.value.uuid, { isEnableWebSearch: false })
      } catch (err) {
        console.error('auto disable webSearch error', err)
      }
      ms.warning(t('chat.deepThinkingAutoCloseWebSearch'))
    }
  }
}, { immediate: true })
</script>

<template>
  <div class="flex flex-col w-full space-x-2 input-tool-bar">
    <div class="flex flex-row space-x-2 items-center py-1">
      <div>
        <LLMSelector :image-size="16" />
      </div>
      <div
        class="flex items-center h-8 px-1 rounded border hover:border-green-600 text-green-600 text-xs"
        :class="{ 'cursor-pointer': isReasoner && isThinkingClosable, 'cursor-not-allowed': !isReasoner || !isThinkingClosable }"
        @click="toogleThinking"
      >
        <template v-if="isReasoner && isThinkingClosable">
          <NPopover v-if="!isMobile" trigger="hover">
            <template #trigger>
              <div class="flex items-center">
                <SvgIcon icon="ri:lightbulb-line" class="text-base mr-1" />
                <NSwitch :value="currCharacter.isEnableThinking" size="small" />
              </div>
            </template>
            <span>{{ t('chat.deepThinking') }}</span>
          </NPopover>
          <template v-else>
            <span class="mr-1">{{ t('chat.deepThinking') }}</span>
            <NSwitch :value="currCharacter.isEnableThinking" size="small" />
          </template>
        </template>
        <template v-if="isReasoner && !isThinkingClosable">
          <NPopover trigger="hover">
            <template #trigger>
              <div class="flex items-center">
                <SvgIcon v-if="!isMobile" icon="ri:lightbulb-line" class="text-base mr-1" />
                <span v-else class="mr-1">{{ t('chat.deepThinking') }}</span>
                <NSwitch :value="true" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.deepThinkingCannotDisable') }} </span>
          </NPopover>
        </template>
        <template v-if="!isReasoner">
          <NPopover trigger="hover">
            <template #trigger>
              <div class="flex items-center">
                <SvgIcon v-if="!isMobile" icon="ri:lightbulb-line" class="text-base mr-1" />
                <span v-else class="mr-1">{{ t('chat.deepThinking') }}</span>
                <NSwitch :value="false" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.deepThinkingNotSupported') }} </span>
          </NPopover>
        </template>
      </div>
      <div
        class="flex items-center h-8 px-1 rounded border hover:border-green-600 text-green-600 text-xs"
        :class="{ 'cursor-pointer': appStore.selectedLLM.isSupportWebSearch, 'cursor-not-allowed': !appStore.selectedLLM.isSupportWebSearch }"
        @click="toogleWebSearch"
      >
        <template v-if="appStore.selectedLLM.isSupportWebSearch">
          <NPopover v-if="!isMobile" trigger="hover">
            <template #trigger>
              <div class="flex items-center">
                <SvgIcon icon="ri:global-line" class="text-base mr-1" />
                <NSwitch :value="currCharacter.isEnableWebSearch" size="small" />
              </div>
            </template>
            <span>{{ t('chat.webSearch') }}</span>
          </NPopover>
          <template v-else>
            <span class="mr-1">{{ t('chat.webSearch') }}</span>
            <NSwitch :value="currCharacter.isEnableWebSearch" size="small" />
          </template>
        </template>
        <template v-if="!appStore.selectedLLM.isSupportWebSearch">
          <NPopover trigger="hover">
            <template #trigger>
              <div class="flex items-center">
                <SvgIcon v-if="!isMobile" icon="ri:global-line" class="text-base mr-1" />
                <span v-else class="mr-1">{{ t('chat.webSearch') }}</span>
                <NSwitch :value="false" size="small" disabled />
              </div>
            </template>
            <span> {{ t('chat.webSearchNotSupported') }} </span>
          </NPopover>
        </template>
      </div>
      <div
        class="flex items-center h-8 px-1 rounded border hover:border-green-600 text-green-600 cursor-pointer"
        @click="toggleUsingContext"
      >
        <NPopover v-if="!isMobile" trigger="hover">
          <template #trigger>
            <div class="flex items-center">
              <SvgIcon icon="ri:chat-history-line" class="text-base mr-1" />
              <NSwitch :value="currCharacter.understandContextEnable" size="small" />
            </div>
          </template>
          <div class="flex flex-col">
            <span>{{ t('chat.understandContext') }}</span>
            <span>{{ currCharacter.understandContextEnable ? t('chat.understandContextOnTip') : t('chat.understandContextOffTip') }}</span>
          </div>
        </NPopover>
        <template v-else>
          <span class="mr-1">{{ t('chat.understandContext') }}</span>
          <NSwitch :value="currCharacter.understandContextEnable" size="small" />
        </template>
      </div>
      <div class="flex h-8 w-8 items-center justify-center rounded border hover:border-green-600 hover:text-green-600 cursor-pointer">
        <NUpload
          :action="`/api/image/upload?token=${token}`" response-type="text" :disabled="!canUploadImage"
          @before-upload="beforeUpload" @finish="handleFinish"
        >
          <NPopover trigger="hover">
            <template #trigger>
              <span class="flex">
                <SvgIcon icon="ri:image-line" class="text-base" />
              </span>
            </template>
            <span> {{ canUploadImage ? t('chat.uploadImageTip') : t('chat.uploadImageNotSupported') }} </span>
          </NPopover>
        </NUpload>
      </div>
      <div
        class="flex items-center overflow-hidden rounded border hover:border-green-600 p-1 h-8 cursor-pointer"
        @click="handleKnowledgeModalShow"
      >
        <NPopover v-if="!isMobile" trigger="hover">
          <template #trigger>
            <div class="flex items-center">
              <SvgIcon icon="ri:book-2-line" class="text-base text-green-600 mr-1" />
              <template v-for="knolwedge in currCharacter.characterKnowledgeList" :key="knolwedge.uuid">
                <span class="text-xs mr-1">{{ knolwedge.title }}</span>
              </template>
            </div>
          </template>
          <span>{{ t('chat.knowledgeBaseLabel') }}</span>
        </NPopover>
        <template v-else>
          <span class="text-xs text-green-600 mr-1">{{ t('chat.knowledgeBaseLabel') }}</span>
          <template v-for="knolwedge in currCharacter.characterKnowledgeList" :key="knolwedge.uuid">
            <span class="text-xs mr-1">{{ knolwedge.title }}</span>
          </template>
          <span v-if="currCharacter.characterKnowledgeList.length === 0" class="text-xs mr-1">{{ t('common.none') }}</span>
        </template>
      </div>
      <div class="flex flex-1 items-center overflow-hidden rounded border hover:border-green-600 cursor-pointer p-1 h-8" @click="handleMcpModalShow">
        <NPopover v-if="!isMobile" trigger="hover">
          <template #trigger>
            <div class="flex items-center">
              <SvgIcon icon="ri:tools-line" class="text-base text-green-600 mr-1" />
              <template v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid">
                <span v-if="currCharacter.mcpIds.includes(userMcp.mcpInfo.id)" class="text-xs mr-1">{{ userMcp.mcpInfo.title
                }}</span>
              </template>
            </div>
          </template>
          <span>{{ t('chat.toolLabel') }}</span>
        </NPopover>
        <template v-else>
          <span class="text-xs text-green-600 mr-1">{{ t('chat.toolLabel') }}</span>
          <template v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid">
            <span v-if="currCharacter.mcpIds.includes(userMcp.mcpInfo.id)" class="text-xs mr-1">{{ userMcp.mcpInfo.title
            }}</span>
          </template>
          <span v-if="currCharacter.mcpIds.length === 0" class="text-xs mr-1">{{ t('common.none') }}</span>
        </template>
      </div>
    </div>
    <NModal
      v-model:show="knowledgeModalShow" display-directive="show" style="width: 90%; max-width: 800px"
      preset="card" :title="t('chat.configCharacterKnowledge')"
    >
      <ConvKnowledgeSelector :tmp-save="false" :character="currCharacter" @submitted="handleKnowledgeSave" />
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

// NUpload 内建包裹层默认 inline-block，svg 会按文本基线对齐而偏上
// Built-in upload wrappers are inline-block by default, aligning the svg to the text baseline
.input-tool-bar .n-upload,
.input-tool-bar .n-upload-trigger {
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
