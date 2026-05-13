<script setup lang='ts'>
import { ref } from 'vue'
import { NRadio, NRadioGroup } from 'naive-ui'
import { t } from '@/locales'

const emit = defineEmits<Emit>()

const publicOrFavor = ref<string>('publicView')

interface Emit {
  (ev: 'displayChange', value: string): void
}
function handleDisplayChange(value: string) {
  emit('displayChange', value)
  publicOrFavor.value = value
}
</script>

<template>
  <header class="left-0 top-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur">
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-10">
      <div class="flex items-center flex-col mx-2">
        <NRadioGroup
          :value="publicOrFavor" name="displayStyleRadioGroup" size="small"
          @update:value="handleDisplayChange"
        >
          <NRadio value="publicView">
            {{ t('gallery.publicImage') }}
          </NRadio>
          <NRadio value="favView">
            {{ t('gallery.myLikes') }}
          </NRadio>
        </NRadioGroup>
      </div>
    </div>
  </header>
</template>
