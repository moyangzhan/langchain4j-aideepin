<script lang="ts" setup>
import { ref, watch } from 'vue'
import { NButton, NCheckbox, NCheckboxGroup, NFlex, NIcon, NInput, NPopconfirm, NRadio, NRadioGroup, NTag, NTooltip, useMessage } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import ConvKnowledgeSelector from '@/views/chat/ConvKnowledgeSelector.vue'
import { useAppStore, useChatStore, useMcpStore } from '@/store'
import { router } from '@/router'
import { debounce } from '@/utils/functions/debounce'
import { emptyConv } from '@/utils/functions'
import api from '@/api'
import { t } from '@/locales'
interface Props {
  conversation: Chat.Conversation
}
interface Emit {
  (ev: 'submitted', show: boolean): void
}
const props = withDefaults(defineProps<Props>(), {})
const emit = defineEmits<Emit>()
const appStore = useAppStore()
const chatStore = useChatStore()
const mcpStore = useMcpStore()
const tmpConv = ref<Chat.Conversation>(emptyConv())
const ms = useMessage()
const submitting = ref<boolean>(false)
const knowledgeModalShow = ref<boolean>(false)

function initEditConv(item: Chat.Conversation) {
  Object.assign(tmpConv.value, item)
  if (!tmpConv.value.audioConfig.voice) {
    tmpConv.value.audioConfig.voice = {
      param_name: '',
      model: '',
      platform: '',
    }
  }
  tmpConv.value.kbIds = []
  tmpConv.value.convKnowledgeList = []
  tmpConv.value.kbIds.push(...item.kbIds)
  tmpConv.value.convKnowledgeList.push(...item.convKnowledgeList)
}
async function handleEdit(event?: KeyboardEvent) {
  event?.stopPropagation()
  if (submitting.value) {
    ms.warning(t('chat.submitting'), {
      duration: 2000,
    })
    return
  }
  if (!tmpConv.value.title) {
    ms.error(t('chat.titleRequired'), {
      duration: 2000,
    })
    return
  }
  try {
    submitting.value = true
    if (!tmpConv.value.uuid) {
      const { data: newConv } = await api.convAdd<Chat.Conversation>(tmpConv.value)
      chatStore.addConvAndActive(newConv)
    } else {
      await api.convEdit(tmpConv.value.uuid, tmpConv.value)
      chatStore.updateConv(tmpConv.value.uuid, tmpConv.value)
    }
  } catch (error: any) {
    console.log('handleEdit error', error)
    if (error.message) {
      ms.error(error.message, {
        duration: 2000,
      })
    }
  } finally {
    submitting.value = false
    emit('submitted', false)
  }
}

function handleRemoveKnowledge(knowledgeId: string) {
  const index = tmpConv.value.convKnowledgeList.findIndex(kb => kb.id === knowledgeId)
  if (index !== -1) {
    tmpConv.value.convKnowledgeList.splice(index, 1)
    const idIndex = tmpConv.value.kbIds.findIndex(id => id === knowledgeId)
    if (idIndex !== -1)
      tmpConv.value.kbIds.splice(idIndex, 1)
  }
}

function handleKnowledgeSelectedChanged(knowledgeIds: string[], knowledgeList: Chat.ConvKnowledge[]) {
  tmpConv.value.kbIds = knowledgeIds
  tmpConv.value.convKnowledgeList = knowledgeList
}

function handleUpdateVoice(name: string) {
  tmpConv.value.audioConfig.voice.param_name = name
  tmpConv.value.audioConfig.voice.model = appStore.ttsSetting.model_name || ''
  tmpConv.value.audioConfig.voice.platform = appStore.ttsSetting.platform || ''
}

function handleDelete(uuid: string, event?: MouseEvent | TouchEvent) {
  event?.stopPropagation()
  try {
    api.convDel(uuid)
    chatStore.deleteConv(uuid)
  } finally {
    emit('submitted', false)
  }
}

function gotoMcp() {
  router.push({ name: 'Mcp' })
  emit('submitted', false)
}

watch(() => props.conversation.uuid, (val) => {
  if (val)
    initEditConv(props.conversation)
}, { immediate: true })

const handleDeleteDebounce = debounce(handleDelete, 600)
</script>

<template>
  <div>
    <div ref="scrollRef" class="h-full overflow-hidden overflow-y-auto max-h-[700px]">
      <div class="flex flex-col space-y-3">
        <div>
          <div class="font-bold">
            {{ t('chat.editConv.nameLabel') }}
          </div>
          <NInput v-model:value="tmpConv.title" type="text" size="large" :placeholder="t('chat.editConv.namePlaceholder')" />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editConv.remarkLabel') }}
          </div>
          <NInput
            v-model:value="tmpConv.remark" type="textarea" :placeholder="t('chat.editConv.remarkPlaceholder')"
            :autosize="{ minRows: 1, maxRows: 10 }"
          />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editConv.roleSettingLabel') }}
          </div>
          <NInput
            v-model:value="tmpConv.aiSystemMessage" type="textarea" :placeholder="t('chat.editConv.roleSettingPlaceholder')"
            :autosize="{ minRows: 1, maxRows: 10 }"
          />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editConv.deepThinking') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>{{ t('chat.editConv.deepThinkingTip1') }}<br></span>
              <span>{{ t('chat.editConv.deepThinkingTip2') }}</span>
            </NTooltip>
          </div>
          <NRadioGroup
            :value="tmpConv.isEnableThinking" name="isEnableThinkingRadio" class="flex flex-col space-y-2"
            size="small" @update:value="(checked) => tmpConv.isEnableThinking = checked"
          >
            <NRadio :value="false">
              {{ t('chat.editConv.off') }}
            </NRadio>
            <NRadio :value="true">
              {{ t('chat.editConv.on') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            <span>{{ t('chat.editConv.knowledgeBase') }}</span>
            <NButton type="primary" size="tiny" text tag="a" @click="knowledgeModalShow = !knowledgeModalShow">
              {{ t('chat.editConv.addMoreKnowledge') }}
            </NButton>
          </div>
          <div v-if="!knowledgeModalShow">
            <div v-if="tmpConv.convKnowledgeList.length === 0" class="pl-6">
              {{ t('common.noData') }}
            </div>
            <NTag
              v-for="convKnowledge in tmpConv.convKnowledgeList" :key="convKnowledge.uuid" closable class="mr-2"
              @close="handleRemoveKnowledge(convKnowledge.id)"
            >
              {{ convKnowledge.title }}
            </NTag>
          </div>
          <div v-show="knowledgeModalShow" class="p-2">
            <ConvKnowledgeSelector
              :tmp-save="true" :conversation="tmpConv"
              @selected-changed="handleKnowledgeSelectedChanged"
            />
          </div>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editConv.mcpServices') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>{{ t('chat.editConv.mcpTip') }}</span>
            </NTooltip>
            <NButton type="primary" size="tiny" text tag="a" @click="gotoMcp">
              {{ t('chat.editConv.goEnableMoreTools') }}
            </NButton>
          </div>
          <NCheckboxGroup v-model:value="tmpConv.mcpIds" class="flex flex-wrap space-x-2">
            <NCheckbox
              v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid" :value="userMcp.mcpInfo.id"
              :label="userMcp.mcpInfo.title"
            />
          </NCheckboxGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editConv.aiReplyFormat') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editConv.formatAutoDesc1') }}<br>
                {{ t('chat.editConv.formatAutoDesc2') }}<br>
                {{ t('chat.editConv.formatTextDesc') }}<br>
                {{ t('chat.editConv.formatAudioDesc') }}
              </span>
            </NTooltip>
          </div>
          <NRadioGroup
            :value="tmpConv.answerContentType" name="answerTypeRadio" class="flex flex-col space-y-2"
            size="small" @update:value="(checked) => tmpConv.answerContentType = checked"
          >
            <NRadio :value="1">
              {{ t('chat.editConv.formatAuto') }}
            </NRadio>
            <NRadio :value="2">
              {{ t('chat.editConv.formatText') }}
            </NRadio>
            <NRadio :value="3">
              {{ t('chat.editConv.formatAudio') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editConv.autoplayAudio') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editConv.autoplayAudioTip') }}
              </span>
            </NTooltip>
          </div>
          <NCheckbox
            :checked="tmpConv.isAutoplayAnswer" :label="t('common.yes')"
            @update:checked="(checked) => tmpConv.isAutoplayAnswer = checked"
          />
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editConv.voiceSelection') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editConv.voiceSelectionTip') }}
              </span>
            </NTooltip>
          </div>
          <div v-if="appStore.ttsSetting.synthesizer_side === 'client'">
            {{ t('chat.editConv.voiceFromBrowser') }}
          </div>
          <NRadioGroup
            v-else-if="appStore.availableVoices.length > 0" :value="tmpConv.audioConfig.voice.param_name"
            name="audioConfigRadio" class="flex flex-col space-y-2" size="small" @update:value="handleUpdateVoice"
          >
            <NRadio
              v-for="voice in appStore.availableVoices" :key="voice.param_name || voice.name"
              :value="voice.param_name || voice.name" :title="voice.name || voice.remark"
            >
              {{ voice.name }}
            </NRadio>
          </NRadioGroup>
          <div v-else>
            {{ t('chat.editConv.noAvailableVoice') }}
          </div>
        </div>
      </div>
    </div>
    <NFlex justify="space-between" class="mt-2">
      <NPopconfirm placement="top" @positive-click.stop="handleDeleteDebounce(tmpConv.uuid, $event)">
        <template #trigger>
          <NButton type="error" text tag="a" :loading="submitting" :disabled="submitting">
            {{ t('common.delete') }}
          </NButton>
        </template>
        {{ t('chat.editConv.confirmDeleteRole', { title: tmpConv.title }) }}
      </NPopconfirm>
      <NButton type="primary" :loading="submitting" :disabled="submitting" @click="handleEdit()">
        {{ t('common.save') }}
      </NButton>
    </NFlex>
  </div>
</template>
