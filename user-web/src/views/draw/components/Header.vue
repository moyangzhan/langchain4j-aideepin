<script setup lang='ts'>
import { ref } from 'vue'
import { NRadio, NRadioGroup } from 'naive-ui'
import { ImageModelSelector } from '@/components/common'
import { t } from '@/locales'

const emit = defineEmits<Emit>()
const selectedDisplayStyle = ref<string>('chatStyle')

interface Emit {
  (ev: 'displayStyleChange', style: string): void
}

function handleDisplayChange(value: string) {
  selectedDisplayStyle.value = value
  emit('displayStyleChange', value)
}
</script>

<template>
  <header class="left-0 top-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur">
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-10">
      <div class="flex items-center flex-col mx-2">
        <ImageModelSelector />
      </div>
      <div class="flex items-center space-x-2">
        <NRadioGroup
          :value="selectedDisplayStyle" name="displayStyleRadioGroup" size="small"
          @update:value="handleDisplayChange"
        >
          <NRadio value="chatStyle">
            {{ t('draw.chatStyle') }}
          </NRadio>
          <NRadio value="galleryStyle">
            {{ t('draw.galleryStyle') }}
          </NRadio>
        </NRadioGroup>
      </div>
    </div>
  </header>
</template>
