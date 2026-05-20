<script setup lang="ts">
import { NButton, NDropdown, NFlex } from 'naive-ui'
import { useAppStore } from '@/store'
import { t } from '@/locales'
const appStore = useAppStore()

function handleSelect(key: string | number) {
  console.log('image model selected', key)
  appStore.setSelectedImageModel(`${key}`)
}
</script>

<template>
  <NDropdown
    size="small" placement="top-end" trigger="click" :show-arrow="true" :options="appStore.imageModels"
    @select="handleSelect"
  >
    <NFlex>
      {{ t('draw.currentModel') }}
      <NButton icon-placement="right" text tag="a" type="primary">
        {{ appStore.selectedImageModel?.modelTitle || appStore.selectedImageModel?.modelName || t('draw.noModel') }}
      </NButton>
    </NFlex>
  </NDropdown>
</template>
