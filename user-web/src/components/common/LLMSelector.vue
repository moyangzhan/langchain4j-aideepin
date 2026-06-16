<script setup lang="ts">
import { computed, h } from 'vue'
import { NButton, NDropdown, NTooltip } from 'naive-ui'
import type { DropdownOption } from 'naive-ui'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import { useAppStore } from '@/store'

const appStore = useAppStore()

// Resolve the current model from the authoritative llms list by id, instead of trusting the
// persisted selectedLLM object (which can drift in localStorage and show a raw modelId as title).
const currentLLM = computed(() => {
  const id = appStore.selectedLLM.modelId
  return (id && appStore.getLLMById(id)) || appStore.selectedLLM
})

function renderLabel(option: DropdownOption) {
  const val = option.value as string
  const llm = appStore.getLLMById(val)
  const modelPlatform = llm?.modelPlatform
  const isUnhealthy = llm?.healthStatus === 'UNHEALTHY'
  const children = [
    h(
      'span',
      {
        class: (isUnhealthy ? 'text-red-500' : 'text-green-500') + ' mr-1',
      },
      '●',
    ),
    h(
      AvatarComponent,
      {
        name: modelPlatform,
        imageSize: 20,
      },
    ),
    h(
      'div',
      {
        class: `ml-1.5${option.disabled ? ' text-gray-400' : ''}`,
      },
      { default: () => option.label as string },
    ),
  ]
  if (isUnhealthy) {
    return h(NTooltip, { placement: 'right' }, {
      trigger: () => h('div', { class: 'flex items-center' }, { default: () => children }),
      default: () => llm?.healthReason || 'Unavailable',
    })
  }
  return h('div', { class: 'flex items-center' }, {
    default: () => children,
  })
}

function handleSelect(key: string | number) {
  appStore.setSelectedLLM(`${key}`)
}
</script>

<template>
  <NDropdown
    size="small" placement="top-start" trigger="click" :show-arrow="true" :render-label="renderLabel"
    :options="appStore.llms" @select="handleSelect"
  >
    <NButton icon-placement="right">
      <AvatarComponent :name="currentLLM.modelPlatform" :image-size="20" class="mr-1" />{{ currentLLM.modelTitle || currentLLM.modelName }}
    </NButton>
  </NDropdown>
</template>
