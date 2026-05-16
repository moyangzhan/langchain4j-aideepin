<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NButton, NDivider, NList, NListItem, NModal, NScrollbar, NTabPane, NTabs, NThing, NTag, NTooltip, useMessage } from 'naive-ui'
import { useAuthStore, useChatStore } from '@/store'
import { emptyConv } from '@/utils/functions'
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
const loadingPresetConvs = ref<boolean>(false)
const loadingRels = ref<boolean>(false)
const tmpConv = ref<Chat.Conversation>(emptyConv())
const showModal = ref<boolean>(false)
const chatStore = useChatStore()
const ms = useMessage()

const groupedPresets = computed(() => {
  const groups: Record<string, Chat.ConversationPreset[]> = {}
  for (const preset of chatStore.presetConvs) {
    const type = preset.type || 'other'
    if (!groups[type])
      groups[type] = []
    groups[type].push(preset)
  }
  const ordered: [string, Chat.ConversationPreset[]][] = []
  for (const type of typeOrder) {
    if (groups[type])
      ordered.push([type, groups[type]])
  }
  return ordered
})

async function searchPresetConvs() {
  if (loadingPresetConvs.value)
    return

  loadingPresetConvs.value = true
  try {
    const { success, data: convs } = await api.searchPresetConvs<PageResponse>()
    if (success)
      chatStore.setPresetConvs(convs.records)
  } finally {
    loadingPresetConvs.value = false
  }
}

async function searchPresetConvRel() {
  if (loadingRels.value)
    return

  loadingRels.value = true
  try {
    const { success, data: rels } = await api.listConvPresetRels<Chat.ConvToPresetRel[]>()
    if (success)
      chatStore.setUsedPresetConv(rels || [])
  } finally {
    loadingRels.value = false
  }
}

function handleSubmitted() {
  showModal.value = false
}

async function handleUsePresetConv(presetConv: Chat.ConversationPreset) {
  if (savingUuids.value.has(presetConv.uuid))
    return

  savingUuids.value.add(presetConv.uuid)
  try {
    const { data: newConv } = await api.convAddByPreset<Chat.Conversation>({ presetConvUuid: presetConv.uuid })
    chatStore.addConvAndActive(newConv)
    chatStore.markPresetConvUsed(presetConv.uuid)

    ms.success(t('chat.copySuccess'), { duration: 2000 })
    if (presetConv.kbTitle)
      ms.success(t('chat.kbCreated', { kbTitle: presetConv.kbTitle }), { duration: 2000 })

    showModal.value = false
  } catch (error: any) {
    console.log('addConv error', error)
    if (error.message) {
      ms.error(error.message, {
        duration: 2000,
      })
    }
  } finally {
    savingUuids.value.delete(presetConv.uuid)
  }

  await searchPresetConvRel()
}

watch(
  () => authStoreRef.value.token,
  async (newVal) => {
    if (newVal) {
      await searchPresetConvs()
      await searchPresetConvRel()
    }
  },
)

onMounted(async () => {
  if (authStoreRef.value.token) {
    await searchPresetConvs()
    await searchPresetConvRel()
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
      <NTabPane name="newConv" :tab="t('chat.newChatButton')">
        <EditConvDetail :conversation="tmpConv" @submitted="handleSubmitted" />
      </NTabPane>
      <NTabPane name="presetConv" :tab="t('chat.presetRole')">
        <NScrollbar style="max-height: 60vh;">
          <template v-for="[type, presets] in groupedPresets" :key="type">
            <NDivider title-placement="left" style="margin: 8px 0 4px;">
              {{ typeLabelMap[type] || type }}
            </NDivider>
            <NList hoverable bordered>
              <NListItem v-for="presetConv in presets" :key="presetConv.id">
                <NThing content-style="margin-top: 6px;">
                  <template #header>
                    {{ presetConv.title }}
                    <NTag v-if="presetConv.used" size="tiny" type="success" style="margin-left: 6px; font-size: 11px;">
                      {{ t('chat.used') }}
                    </NTag>
                    <NTooltip v-if="presetConv.kbTitle">
                      <template #trigger>
                        <NTag size="tiny" type="info" style="margin-left: 6px; font-size: 11px;">
                          {{ t('chat.kbAttached') }}
                        </NTag>
                      </template>
                      {{ t('chat.kbAutoCreateTip') }}
                    </NTooltip>
                  </template>
                  {{ presetConv.remark }}
                </NThing>
                <template #suffix>
                  <NButton size="small" :loading="savingUuids.has(presetConv.uuid)" :disabled="savingUuids.has(presetConv.uuid)" @click="handleUsePresetConv(presetConv)">
                    {{ savingUuids.has(presetConv.uuid) ? t('chat.copying') : t('chat.use') }}
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
