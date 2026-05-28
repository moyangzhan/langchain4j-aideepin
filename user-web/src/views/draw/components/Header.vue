<script setup lang='ts'>
import { computed, ref } from 'vue'
import { NButton, NDropdown, NRadio, NRadioGroup } from 'naive-ui'
import { ApiKeyModal, ImageModelSelector, SvgIcon } from '@/components/common'
import { t } from '@/locales'

const emit = defineEmits<Emit>()
const selectedDisplayStyle = ref<string>('chatStyle')
const showApiKeyModal = ref(false)

interface Emit {
  (ev: 'displayStyleChange', style: string): void
}

const menuOptions = computed(() => [
  { label: t('extApi.apiAccess'), key: 'api' },
])

function handleMenuSelect(key: string) {
  if (key === 'api')
    showApiKeyModal.value = true
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
        <div class="w-px h-4 bg-gray-300 dark:bg-gray-600" />
        <NDropdown :options="menuOptions" trigger="click" @select="handleMenuSelect">
          <NButton quaternary circle size="small">
            <template #icon>
              <SvgIcon icon="ri:more-fill" />
            </template>
          </NButton>
        </NDropdown>
      </div>
    </div>
  </header>

  <ApiKeyModal v-model:show="showApiKeyModal" type="draw" uuid="" :title="t('draw.title')" />
</template>
