<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NButton, NDivider, NList, NListItem, NModal, NScrollbar, NTabPane, NTabs, NThing, NTag, NTooltip, useMessage } from 'naive-ui'
import { useAuthStore, useChatStore } from '@/store'
import { emptyCharacter } from '@/utils/functions'
import EditConvDetail from '@/views/chat/components/Header/EditConvDetail.vue'
import api from '@/api'
import { t } from '@/locales'

const typeOrder = [
  'technology', 'creative', 'education', 'business', 'professional',
  'design', 'marketing', 'service', 'administration', 'utility', 'other',
]

const typeLabelMap = computed<Record<string, string>>(() => ({
  technology: t('chat.presetTypeTechnology'),
  creative: t('chat.presetTypeCreative'),
  education: t('chat.presetTypeEducation'),
  business: t('chat.presetTypeBusiness'),
  professional: t('chat.presetTypeProfessional'),
  design: t('chat.presetTypeDesign'),
  marketing: t('chat.presetTypeMarketing'),
  service: t('chat.presetTypeService'),
  administration: t('chat.presetTypeAdministration'),
  utility: t('chat.presetTypeUtility'),
  other: t('chat.presetTypeOther'),
}))

const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const savingUuids = ref<Set<string>>(new Set())
const loadingPresetCharacters = ref<boolean>(false)
const loadingRels = ref<boolean>(false)
const tmpCharacter = ref<Chat.Character>(emptyCharacter())
const showModal = ref<boolean>(false)
const chatStore = useChatStore()
const ms = useMessage()

const groupedPresets = computed(() => {
  const groups: Record<string, Chat.CharacterPreset[]> = {}
  for (const preset of chatStore.presetCharacters) {
    const type = preset.type || 'other'
    if (!groups[type])
      groups[type] = []
    groups[type].push(preset)
  }
  const ordered: [string, Chat.CharacterPreset[]][] = []
  for (const type of typeOrder) {
    if (groups[type])
      ordered.push([type, groups[type]])
  }
  return ordered
})

async function searchPresetCharacters() {
  if (loadingPresetCharacters.value)
    return

  loadingPresetCharacters.value = true
  try {
    const { success, data: characters } = await api.searchPresetCharacters<PageResponse>()
    if (success)
      chatStore.setPresetCharacters(characters.records)
  } finally {
    loadingPresetCharacters.value = false
  }
}

async function searchPresetCharacterRel() {
  if (loadingRels.value)
    return

  loadingRels.value = true
  try {
    const { success, data: rels } = await api.listCharacterPresetRels<Chat.CharacterToPresetRel[]>()
    if (success)
      chatStore.setUsedPresetCharacter(rels || [])
  } finally {
    loadingRels.value = false
  }
}

function handleSubmitted() {
  showModal.value = false
}

async function handleUsePresetCharacter(presetCharacter: Chat.CharacterPreset) {
  if (savingUuids.value.has(presetCharacter.uuid))
    return

  savingUuids.value.add(presetCharacter.uuid)
  try {
    const { data: newCharacter } = await api.characterAddByPreset<Chat.Character>({ presetCharacterUuid: presetCharacter.uuid })
    chatStore.addCharacterAndActive(newCharacter)
    chatStore.markPresetCharacterUsed(presetCharacter.uuid)

    ms.success(t('chat.copySuccess'), { duration: 2000 })
    if (presetCharacter.kbTitle)
      ms.success(t('chat.kbCreated', { kbTitle: presetCharacter.kbTitle }), { duration: 2000 })

    showModal.value = false
  } catch (error: any) {
    console.log('addCharacter error', error)
    if (error.message) {
      ms.error(error.message, {
        duration: 2000,
      })
    }
  } finally {
    savingUuids.value.delete(presetCharacter.uuid)
  }

  await searchPresetCharacterRel()
}

watch(
  () => authStoreRef.value.token,
  async (newVal) => {
    if (newVal) {
      await searchPresetCharacters()
      await searchPresetCharacterRel()
    }
  },
)

onMounted(async () => {
  if (authStoreRef.value.token) {
    await searchPresetCharacters()
    await searchPresetCharacterRel()
  }
})

function toggleModal() {
  showModal.value = !showModal.value
}
defineExpose({ toggleModal })
</script>

<template>
  <NModal v-model:show="showModal" style="min-width:200px; width: 60%;" preset="card">
    <NTabs type="line" justify-content="space-evenly" animated>
      <NTabPane name="newCharacter" :tab="t('chat.newChatButton')">
        <EditConvDetail :character="tmpCharacter" @submitted="handleSubmitted" />
      </NTabPane>
      <NTabPane name="presetCharacter" :tab="t('chat.presetRole')">
        <NScrollbar style="max-height: 60vh;">
          <template v-for="[type, presets] in groupedPresets" :key="type">
            <NDivider title-placement="left" style="margin: 8px 0 4px;">
              {{ typeLabelMap[type] || type }}
            </NDivider>
            <NList hoverable bordered>
              <NListItem v-for="presetCharacter in presets" :key="presetCharacter.id">
                <NThing content-style="margin-top: 6px;">
                  <template #header>
                    {{ presetCharacter.title }}
                    <NTag v-if="presetCharacter.used" size="tiny" type="success" style="margin-left: 6px; font-size: 11px;">
                      {{ t('chat.used') }}
                    </NTag>
                    <NTooltip v-if="presetCharacter.kbTitle">
                      <template #trigger>
                        <NTag size="tiny" type="info" style="margin-left: 6px; font-size: 11px;">
                          {{ t('chat.kbAttached') }}
                        </NTag>
                      </template>
                      {{ t('chat.kbAutoCreateTip') }}
                    </NTooltip>
                  </template>
                  {{ presetCharacter.remark }}
                </NThing>
                <template #suffix>
                  <NButton size="small" :loading="savingUuids.has(presetCharacter.uuid)" :disabled="savingUuids.has(presetCharacter.uuid)" @click="handleUsePresetCharacter(presetCharacter)">
                    {{ savingUuids.has(presetCharacter.uuid) ? t('chat.copying') : t('chat.use') }}
                  </NButton>
                </template>
              </NListItem>
            </NList>
          </template>
        </NScrollbar>
      </NTabPane>
    </NTabs>
  </NModal>
</template>
