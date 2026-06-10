<script setup lang="ts">
import { h } from 'vue'
import { NButton, NDropdown, NTooltip } from 'naive-ui'
import type { VNode } from 'vue'
import type { DropdownGroupOption, DropdownOption } from 'naive-ui'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import { useAppStore } from '@/store'
import { t } from '@/locales'

const appStore = useAppStore()

function renderOption({ node, option }: { node: VNode; option: DropdownOption | DropdownGroupOption }) {
  if (option.enable && option.isFree) {
    return h(NTooltip, { placement: 'left' }, {
      trigger: () => node,
      default: () => t('setting.tokenQuotaUnlimited'),
    })
  } else {
    return node
  }
}
function renderLabel(option: DropdownOption) {
  const val = option.value as string
  const llm = appStore.getLLMById(val)
  const modelPlatform = llm?.modelPlatform
  const isUnhealthy = llm?.healthStatus === 'UNHEALTHY'
  const children = [
    h(
      'span',
      {
        class: option.isFree ? 'text-green-500' : 'text-orange-500',
      },
      '⨀ ',
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
      { default: () => (option.label as string) + (isUnhealthy ? ' (🚫)' : '') },
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
    :render-option="renderOption" :options="appStore.llms" @select="handleSelect"
  >
    <NButton icon-placement="right">
      <AvatarComponent :name="appStore.selectedLLM.modelPlatform" :image-size="20" class="mr-1" />{{ appStore.selectedLLM.modelTitle || appStore.selectedLLM.modelName }}
    </NButton>
  </NDropdown>
</template>
