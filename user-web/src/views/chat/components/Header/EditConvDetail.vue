<script lang="ts" setup>
import { ref, watch } from 'vue'
import { NButton, NCheckbox, NCheckboxGroup, NFlex, NIcon, NInput, NPopconfirm, NRadio, NRadioGroup, NTag, NTooltip, useMessage } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import ConvKnowledgeSelector from '@/views/chat/ConvKnowledgeSelector.vue'
import { useAppStore, useChatStore, useMcpStore } from '@/store'
import { router } from '@/router'
import { debounce } from '@/utils/functions/debounce'
import { emptyCharacter } from '@/utils/functions'
import api from '@/api'
import { t } from '@/locales'
interface Props {
  character: Chat.Character
}
interface Emit {
  (ev: 'submitted', show: boolean): void
}
const props = withDefaults(defineProps<Props>(), {})
const emit = defineEmits<Emit>()
const appStore = useAppStore()
const chatStore = useChatStore()
const mcpStore = useMcpStore()
const tmpCharacter = ref<Chat.Character>(emptyCharacter())
const ms = useMessage()
const submitting = ref<boolean>(false)
const knowledgeModalShow = ref<boolean>(false)

function initEditCharacter(item: Chat.Character) {
  Object.assign(tmpCharacter.value, item)
  if (!tmpCharacter.value.audioConfig.voice) {
    tmpCharacter.value.audioConfig.voice = {
      param_name: '',
      model: '',
      platform: '',
    }
  }
  tmpCharacter.value.kbIds = []
  tmpCharacter.value.characterKnowledgeList = []
  tmpCharacter.value.kbIds.push(...item.kbIds)
  tmpCharacter.value.characterKnowledgeList.push(...item.characterKnowledgeList)
}
async function handleEdit(event?: KeyboardEvent) {
  event?.stopPropagation()
  if (submitting.value) {
    ms.warning(t('chat.submitting'), {
      duration: 2000,
    })
    return
  }
  if (!tmpCharacter.value.title) {
    ms.error(t('chat.titleRequired'), {
      duration: 2000,
    })
    return
  }
  try {
    submitting.value = true
    if (!tmpCharacter.value.uuid) {
      const { data: newCharacter } = await api.characterAdd<Chat.Character>(tmpCharacter.value)
      chatStore.addCharacterAndActive(newCharacter)
    } else {
      await api.characterEdit(tmpCharacter.value.uuid, tmpCharacter.value)
      chatStore.updateCharacter(tmpCharacter.value.uuid, tmpCharacter.value)
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
  const index = tmpCharacter.value.characterKnowledgeList.findIndex(kb => kb.id === knowledgeId)
  if (index !== -1) {
    tmpCharacter.value.characterKnowledgeList.splice(index, 1)
    const idIndex = tmpCharacter.value.kbIds.findIndex(id => id === knowledgeId)
    if (idIndex !== -1)
      tmpCharacter.value.kbIds.splice(idIndex, 1)
  }
}

function handleKnowledgeSelectedChanged(knowledgeIds: string[], knowledgeList: Chat.CharacterKnowledge[]) {
  tmpCharacter.value.kbIds = knowledgeIds
  tmpCharacter.value.characterKnowledgeList = knowledgeList
}

function handleUpdateVoice(name: string) {
  tmpCharacter.value.audioConfig.voice.param_name = name
  tmpCharacter.value.audioConfig.voice.model = appStore.ttsSetting.model_name || ''
  tmpCharacter.value.audioConfig.voice.platform = appStore.ttsSetting.platform || ''
}

function handleDelete(uuid: string, event?: MouseEvent | TouchEvent) {
  event?.stopPropagation()
  try {
    api.characterDel(uuid)
    chatStore.deleteCharacter(uuid)
  } finally {
    emit('submitted', false)
  }
}

function gotoMcp() {
  router.push({ name: 'Mcp' })
  emit('submitted', false)
}

watch(() => props.character.uuid, (val) => {
  if (val)
    initEditCharacter(props.character)
}, { immediate: true })

const handleDeleteDebounce = debounce(handleDelete, 600)
</script>

<template>
  <div>
    <div ref="scrollRef" class="h-full overflow-hidden overflow-y-auto max-h-[700px]">
      <div class="flex flex-col space-y-3">
        <div>
          <div class="font-bold">
            {{ t('chat.editCharacterDetail.nameLabel') }}
          </div>
          <NInput v-model:value="tmpCharacter.title" type="text" size="large" :placeholder="t('chat.editCharacterDetail.namePlaceholder')" />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editCharacterDetail.remarkLabel') }}
          </div>
          <NInput
            v-model:value="tmpCharacter.remark" type="textarea" :placeholder="t('chat.editCharacterDetail.remarkPlaceholder')"
            :autosize="{ minRows: 1, maxRows: 10 }"
          />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editCharacterDetail.roleSettingLabel') }}
          </div>
          <NInput
            v-model:value="tmpCharacter.aiSystemMessage" type="textarea" :placeholder="t('chat.editCharacterDetail.roleSettingPlaceholder')"
            :autosize="{ minRows: 1, maxRows: 10 }"
          />
        </div>
        <div>
          <div class="font-bold">
            {{ t('chat.editCharacterDetail.deepThinking') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>{{ t('chat.editCharacterDetail.deepThinkingTip1') }}<br></span>
              <span>{{ t('chat.editCharacterDetail.deepThinkingTip2') }}</span>
            </NTooltip>
          </div>
          <NRadioGroup
            :value="tmpCharacter.isEnableThinking" name="isEnableThinkingRadio" class="flex flex-col space-y-2"
            size="small" @update:value="(checked) => tmpCharacter.isEnableThinking = checked"
          >
            <NRadio :value="false">
              {{ t('chat.editCharacterDetail.off') }}
            </NRadio>
            <NRadio :value="true">
              {{ t('chat.editCharacterDetail.on') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            <span>{{ t('chat.editCharacterDetail.knowledgeBase') }}</span>
            <NButton type="primary" size="tiny" text tag="a" @click="knowledgeModalShow = !knowledgeModalShow">
              {{ t('chat.editCharacterDetail.addMoreKnowledge') }}
            </NButton>
          </div>
          <div v-if="!knowledgeModalShow">
            <div v-if="tmpCharacter.characterKnowledgeList.length === 0" class="pl-6">
              {{ t('common.noData') }}
            </div>
            <NTag
              v-for="characterKnowledge in tmpCharacter.characterKnowledgeList" :key="characterKnowledge.uuid" closable class="mr-2"
              @close="handleRemoveKnowledge(characterKnowledge.id)"
            >
              {{ characterKnowledge.title }}
            </NTag>
          </div>
          <div v-show="knowledgeModalShow" class="p-2">
            <ConvKnowledgeSelector
              :tmp-save="true" :character="tmpCharacter"
              @selected-changed="handleKnowledgeSelectedChanged"
            />
          </div>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editCharacterDetail.mcpServices') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>{{ t('chat.editCharacterDetail.mcpTip') }}</span>
            </NTooltip>
            <NButton type="primary" size="tiny" text tag="a" @click="gotoMcp">
              {{ t('chat.editCharacterDetail.goEnableMoreTools') }}
            </NButton>
          </div>
          <NCheckboxGroup v-model:value="tmpCharacter.mcpIds" class="flex flex-wrap space-x-2">
            <NCheckbox
              v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid" :value="userMcp.mcpInfo.id"
              :label="userMcp.mcpInfo.title"
            />
          </NCheckboxGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editCharacterDetail.aiReplyFormat') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editCharacterDetail.formatAutoDesc1') }}<br>
                {{ t('chat.editCharacterDetail.formatAutoDesc2') }}<br>
                {{ t('chat.editCharacterDetail.formatTextDesc') }}<br>
                {{ t('chat.editCharacterDetail.formatAudioDesc') }}
              </span>
            </NTooltip>
          </div>
          <NRadioGroup
            :value="tmpCharacter.answerContentType" name="answerTypeRadio" class="flex flex-col space-y-2"
            size="small" @update:value="(checked) => tmpCharacter.answerContentType = checked"
          >
            <NRadio :value="1">
              {{ t('chat.editCharacterDetail.formatAuto') }}
            </NRadio>
            <NRadio :value="2">
              {{ t('chat.editCharacterDetail.formatText') }}
            </NRadio>
            <NRadio :value="3">
              {{ t('chat.editCharacterDetail.formatAudio') }}
            </NRadio>
          </NRadioGroup>
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editCharacterDetail.autoplayAudio') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editCharacterDetail.autoplayAudioTip') }}
              </span>
            </NTooltip>
          </div>
          <NCheckbox
            :checked="tmpCharacter.isAutoplayAnswer" :label="t('common.yes')"
            @update:checked="(checked) => tmpCharacter.isAutoplayAnswer = checked"
          />
        </div>
        <div class="flex flex-col space-y-2">
          <div class="flex space-x-2 font-bold">
            {{ t('chat.editCharacterDetail.voiceSelection') }}
            <NTooltip trigger="hover">
              <template #trigger>
                <NIcon style="margin-top: 0.2rem">
                  <QuestionCircle16Regular />
                </NIcon>
              </template>
              <span>
                {{ t('chat.editCharacterDetail.voiceSelectionTip') }}
              </span>
            </NTooltip>
          </div>
          <div v-if="appStore.ttsSetting.synthesizer_side === 'client'">
            {{ t('chat.editCharacterDetail.voiceFromBrowser') }}
          </div>
          <NRadioGroup
            v-else-if="appStore.availableVoices.length > 0" :value="tmpCharacter.audioConfig.voice.param_name"
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
            {{ t('chat.editCharacterDetail.noAvailableVoice') }}
          </div>
        </div>
      </div>
    </div>
    <NFlex justify="space-between" class="mt-2">
      <NPopconfirm placement="top" @positive-click.stop="handleDeleteDebounce(tmpCharacter.uuid, $event)">
        <template #trigger>
          <NButton type="error" text tag="a" :loading="submitting" :disabled="submitting">
            {{ t('common.delete') }}
          </NButton>
        </template>
        {{ t('chat.editCharacterDetail.confirmDeleteRole', { title: tmpCharacter.title }) }}
      </NPopconfirm>
      <NButton type="primary" :loading="submitting" :disabled="submitting" @click="handleEdit()">
        {{ t('common.save') }}
      </NButton>
    </NFlex>
  </div>
</template>
