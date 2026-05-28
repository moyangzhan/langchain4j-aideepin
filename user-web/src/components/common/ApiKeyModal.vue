<script setup lang='ts'>
import { computed, ref, watch } from 'vue'
import { NAlert, NButton, NFlex, NH4, NInput, NModal, NPopconfirm, NSpace, useMessage } from 'naive-ui'
import ApiDocPanel from './ApiDocPanel.vue'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  show: boolean
  type: string
  uuid: string
  title: string
  wfInputDefs?: Workflow.NodeIODefinition[]
}
interface Emit {
  (ev: 'update:show', value: boolean): void
}
const props = withDefaults(defineProps<Props>(), {
  show: false,
  type: '',
  uuid: '',
  title: '',
})
const emit = defineEmits<Emit>()
const ms = useMessage()

const innerShow = ref(false)
const loading = ref(false)
const maskedKey = ref('')
const rawKey = ref('')
const showingRaw = ref(false)
const justGenerated = ref(false)
const canManage = ref(false)

// 判断是否为用户级 API Key 类型
const isUserLevelType = computed(() => ['draw', 'mcp'].includes(props.type))

watch(() => props.show, (val) => {
  innerShow.value = val
  if (val)
    fetchKeyInfo()
})

watch(() => innerShow.value, (val) => {
  if (!val) {
    emit('update:show', false)
    rawKey.value = ''
    showingRaw.value = false
    justGenerated.value = false
  }
})

async function fetchKeyInfo() {
  if (loading.value)
    return
  loading.value = true
  try {
    let data
    if (isUserLevelType.value)
      data = (await api.extApiKeyUserInfo(props.type)).data
    else
      data = (await api.extApiKeyInfo(props.type, props.uuid)).data
    maskedKey.value = data?.maskedKey ?? ''
    canManage.value = data?.canManage ?? false
    rawKey.value = ''
    showingRaw.value = false
    justGenerated.value = false
  } catch {
    maskedKey.value = ''
    rawKey.value = ''
    ms.error(t('common.wrong'))
  } finally {
    loading.value = false
  }
}

async function handleGenerate() {
  loading.value = true
  try {
    let data
    if (isUserLevelType.value)
      data = (await api.extApiKeyUserGenerate(props.type)).data
    else
      data = (await api.extApiKeyGenerate(props.type, props.uuid)).data
    maskedKey.value = data.maskedKey
    rawKey.value = data.rawKey
    justGenerated.value = true
    showingRaw.value = true
  } catch {
    ms.error(t('common.wrong'))
  } finally {
    loading.value = false
  }
}

async function handleReveal() {
  if (showingRaw.value) {
    showingRaw.value = false
    rawKey.value = ''
    return
  }
  loading.value = true
  try {
    let data
    if (isUserLevelType.value)
      data = (await api.extApiKeyUserReveal(props.type)).data
    else
      data = (await api.extApiKeyReveal(props.type, props.uuid)).data
    rawKey.value = data.rawKey
    showingRaw.value = true
  } catch {
    ms.error(t('common.wrong'))
  } finally {
    loading.value = false
  }
}

async function handleCopy() {
  if (!rawKey.value)
    return
  try {
    await navigator.clipboard.writeText(rawKey.value)
    ms.success(t('extApi.copySuccess'))
  } catch {
    ms.error(t('common.wrong'))
  }
}
</script>

<template>
  <NModal v-model:show="innerShow" :title="`${title} - ${t('extApi.apiAccess')}`" style="width: 90%; max-width: 900px" preset="card" :mask-closable="false" :auto-focus="false" content-style="max-height: 80vh; overflow-y: auto;">
    <NSpace vertical :size="16">
      <NH4 v-if="canManage" style="margin: 0">
        {{ t('extApi.apiKeyLabel') }}
      </NH4>
      <NAlert v-if="justGenerated" type="warning" :show-icon="true">
        {{ t('extApi.closeWarning') }}
      </NAlert>

      <div v-if="canManage">
        <!-- No key yet -->
        <NFlex v-if="!maskedKey" justify="center" :size="12">
          <NButton type="primary" :loading="loading" @click="handleGenerate">
            {{ t('extApi.generateKey') }}
          </NButton>
        </NFlex>

        <!-- Key exists -->
        <template v-else>
          <NInput :value="showingRaw ? rawKey : maskedKey" readonly :style="{ backgroundColor: 'var(--n-color-disabled)', marginBottom: '8px' }" />
          <NFlex justify="end" :size="8">
            <NButton v-if="showingRaw" size="small" @click="handleCopy">
              {{ t('extApi.copyKey') }}
            </NButton>
            <NButton size="small" @click="handleReveal">
              {{ showingRaw ? t('extApi.hideKey') : t('extApi.viewKey') }}
            </NButton>
            <NPopconfirm @positive-click="handleGenerate">
              <template #trigger>
                <NButton size="small" type="warning">
                  {{ t('extApi.regenerateKey') }}
                </NButton>
              </template>
              {{ t('extApi.regenerateConfirm') }}
            </NPopconfirm>
          </NFlex>
        </template>
      </div>

      <!-- API Documentation -->
      <ApiDocPanel :type="props.type" :uuid="props.uuid" :wf-input-defs="props.wfInputDefs" />
    </NSpace>
  </NModal>
</template>

<style scoped>
:deep(.n-input--readonly:hover) {
  border-color: var(--n-border-color) !important;
}
</style>
