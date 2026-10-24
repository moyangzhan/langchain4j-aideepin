<script setup lang="ts">
import { computed } from 'vue'
import { NButton, NDropdown, NFlex } from 'naive-ui'
import { useAppStore } from '@/store'
import { t } from '@/locales'
const appStore = useAppStore()

// 画图模型准入:仅展示声明了文本输入(input_types 含 text)的模型,即文生图能力
// Draw admission: only models declaring text input (input_types contains text), i.e. text-to-image capable
const options = computed(() => {
  return appStore.imageModels.filter(item => item.inputTypes?.split(',').includes('text') ?? false)
})

function handleSelect(key: string | number) {
  console.log('image model selected', key)
  appStore.setSelectedImageModel(`${key}`)
}
</script>

<template>
  <NDropdown
    size="small" placement="top-end" trigger="click" :show-arrow="true" :options="options"
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
