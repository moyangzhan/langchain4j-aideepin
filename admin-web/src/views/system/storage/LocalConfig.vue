<template>
  <div>{{ t('system.localStorageContent') }}</div>
  <br />
  <n-button type="primary" :loading="loading" @click="activeSetting">{{
    t('system.useThisStorage')
  }}</n-button>
</template>
<script lang="ts" setup>
  import { ref } from 'vue'
  import { useMessage } from 'naive-ui'
  import api from '@/api/sysConfig.js'
  import { t } from '@/locales'

  interface Emit {
    (e: 'reload'): void
  }
  const emit = defineEmits<Emit>()
  const loading = ref<boolean>(false)
  const message = useMessage()
  async function activeSetting() {
    if (loading.value) return
    loading.value = true
    setTimeout(() => {
      loading.value = false
    }, 2000)
    await api.edit({ name: 'storage_location', value: '1' })
    message.success(t('system.localStorageSuccess'))
    emit('reload')
  }
</script>
