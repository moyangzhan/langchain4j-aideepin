<template>
  <n-card :bordered="false" size="small" :title="t('route.fileGcConfig')" class="proCard">
    <n-alert type="info" :bordered="true" class="mb-3">
      {{ t('fileGc.desc') }}
    </n-alert>
    <n-form label-placement="left" label-width="120">
      <n-form-item :label="t('fileGc.switchLabel')">
        <n-switch v-model:value="enabled" :loading="loading" @update:value="onSwitch" />
        <span class="ml-2">{{ enabled ? t('fileGc.enabled') : t('fileGc.disabled') }}</span>
      </n-form-item>
    </n-form>
  </n-card>
</template>

<script lang="ts" setup>
  import { onMounted, ref } from 'vue'
  import api from '@/api/sysConfig'
  import { t } from '@/locales'

  const enabled = ref(false)
  const loading = ref(false)

  async function load() {
    loading.value = true
    try {
      const { data } = await api.search({ names: ['file_gc_enabled'] }, 1, 10)
      enabled.value = (data?.records || []).some((item) => item.name === 'file_gc_enabled' && item.value === 'true')
    } catch (e) {
      // 加载失败保持关闭显示，重进页面可重试 | keep showing "off" on failure; re-entering retries
    } finally {
      loading.value = false
    }
  }

  async function onSwitch(value: boolean) {
    loading.value = true
    try {
      const resp = await api.edit({ name: 'file_gc_enabled', value: value ? 'true' : 'false' })
      if (resp.success) {
        window['$message'].success(t('common.editSuccess'))
      } else {
        enabled.value = !value
      }
    } catch (e) {
      enabled.value = !value
    } finally {
      loading.value = false
    }
  }

  onMounted(() => {
    load()
  })
</script>

<style lang="less" scoped></style>
