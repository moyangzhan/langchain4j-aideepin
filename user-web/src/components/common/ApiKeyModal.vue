<script setup lang='ts'>
import { ref, watch } from 'vue'
import { NAlert, NButton, NFlex, NInput, NModal, NPopconfirm, NSpace, useMessage } from 'naive-ui'
import api from '@/api'
import { t } from '@/locales'
import ApiDocPanel from './ApiDocPanel.vue'

interface Props {
  show: boolean
  type: string
  uuid: string
  title: string
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

watch(() => props.show, (val) => {
  innerShow.value = val
  if (val && props.uuid)
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
    const { data } = await api.openApiKeyInfo<{ rawKey: string | null, maskedKey: string | null }>(props.type, props.uuid)
    maskedKey.value = data?.maskedKey ?? ''
    rawKey.value = ''
    showingRaw.value = false
    justGenerated.value = false
  }
  catch {
    maskedKey.value = ''
    rawKey.value = ''
    ms.error(t('common.wrong'))
  }
  finally {
    loading.value = false
  }
}

async function handleGenerate() {
  loading.value = true
  try {
    const { data } = await api.openApiKeyGenerate<{ rawKey: string, maskedKey: string }>(props.type, props.uuid)
    maskedKey.value = data.maskedKey
    rawKey.value = data.rawKey
    justGenerated.value = true
    showingRaw.value = true
  }
  catch {
    ms.error(t('common.wrong'))
  }
  finally {
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
    const { data } = await api.openApiKeyReveal<{ rawKey: string, maskedKey: string }>(props.type, props.uuid)
    rawKey.value = data.rawKey
    showingRaw.value = true
  }
  catch {
    ms.error(t('common.wrong'))
  }
  finally {
    loading.value = false
  }
}

async function handleCopy() {
  if (!rawKey.value)
    return
  try {
    await navigator.clipboard.writeText(rawKey.value)
    ms.success(t('openApi.copySuccess'))
  }
  catch {
    ms.error(t('common.wrong'))
  }
}
</script>

<template>
  <NModal v-model:show="innerShow" :title="`${title} - ${t('openApi.apiAccess')}`" style="width: 90%; max-width: 640px" preset="card" :mask-closable="false">
    <NSpace vertical :size="16">
      <NAlert v-if="justGenerated" type="warning" :show-icon="true">
        {{ t('openApi.closeWarning') }}
      </NAlert>

      <!-- No key yet -->
      <NFlex v-if="!maskedKey" justify="center" :size="12">
        <NButton type="primary" :loading="loading" @click="handleGenerate">
          {{ t('openApi.generateKey') }}
        </NButton>
      </NFlex>

      <!-- Key exists -->
      <template v-else>
        <NFlex align="center" :size="8">
          <NInput :value="showingRaw ? rawKey : maskedKey" readonly :type="showingRaw ? 'textarea' : 'text'" />
          <NButton v-if="showingRaw" size="small" @click="handleCopy">
            {{ t('openApi.copyKey') }}
          </NButton>
        </NFlex>
        <NFlex justify="end" :size="8">
          <NButton size="small" @click="handleReveal">
            {{ showingRaw ? t('openApi.hideKey') : t('openApi.viewKey') }}
          </NButton>
          <NPopconfirm @positive-click="handleGenerate">
            <template #trigger>
              <NButton size="small" type="warning">
                {{ t('openApi.regenerateKey') }}
              </NButton>
            </template>
            {{ t('openApi.regenerateConfirm') }}
          </NPopconfirm>
        </NFlex>
      </template>

      <!-- API Documentation -->
      <ApiDocPanel :type="props.type" />
    </NSpace>
  </NModal>
</template>
