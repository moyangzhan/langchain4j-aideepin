<script setup lang='ts'>
import { onMounted, ref, watch } from 'vue'
import { NButton, NList, NListItem, NModal, NScrollbar, NTabPane, NTabs, NThing, useMessage } from 'naive-ui'
import { useAuthStore, useChatStore } from '@/store'
import { emptyConv } from '@/utils/functions'
import EditConvDetail from '@/views/chat/components/Header/EditConvDetail.vue'
import api from '@/api'
import { t } from '@/locales'
const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const convSaving = ref<boolean>(false)
const loadingPresetConvs = ref<boolean>(false)
const loadingRels = ref<boolean>(false)
const tmpConv = ref<Chat.Conversation>(emptyConv())
const showModal = ref<boolean>(false)
const chatStore = useChatStore()
const ms = useMessage()

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
    if (success && rels && rels.length > 0)
      chatStore.setUsedPresetConv(rels)
  } finally {
    loadingRels.value = false
  }
}

function handleSubmitted() {
  showModal.value = false
}

async function handleUsePresetConv(presetConvUuid: string) {
  if (convSaving.value)
    return

  convSaving.value = true
  try {
    const { data: newConv } = await api.convAddByPreset<Chat.Conversation>({ presetConvUuid })
    chatStore.addConvAndActive(newConv)

    showModal.value = false
  } catch (error: any) {
    console.log('addConv error', error)
    if (error.message) {
      ms.error(error.message, {
        duration: 2000,
      })
    }
  } finally {
    convSaving.value = false
  }

  await searchPresetConvRel()
}

watch(
  () => authStoreRef.value.token,
  (newVal) => {
    if (newVal) {
      searchPresetConvs()
      searchPresetConvRel()
    }
  },
)

onMounted(async () => {
  if (authStoreRef.value.token) {
    searchPresetConvs()
    searchPresetConvRel()
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
        <NScrollbar class="max-h-96">
          <NList hoverable bordered>
            <NListItem v-for="presetConv in chatStore.presetConvs" :key="presetConv.id">
              <NThing :title="presetConv.title" content-style="margin-top: 10px;">
                {{ presetConv.remark }}
              </NThing>
              <template #suffix>
                <template v-if="presetConv.used">
                  <NButton size="small" disabled>
                    {{ t('chat.used') }}
                  </NButton>
                </template>
                <template v-else>
                  <NButton size="small" @click="handleUsePresetConv(presetConv.uuid)">
                    {{ t('chat.use') }}
                  </NButton>
                </template>
              </template>
            </NListItem>
          </NList>
        </NScrollbar>
      </NTabPane>
    </NTabs>
  </NModal>
</template>
