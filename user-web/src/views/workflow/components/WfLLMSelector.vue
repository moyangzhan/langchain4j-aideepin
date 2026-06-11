<script setup lang="ts">
import { h, ref } from 'vue'
import { NSelect, NTooltip } from 'naive-ui'
import type { SelectOption } from 'naive-ui'
import type { VNodeChild } from 'vue'
import { useAppStore } from '@/store'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import { emptyAiModel } from '@/utils/functions'

interface Props {
  modelPlatform: string
  modelName: string
}
interface Emit {
  (e: 'llmSelected', aiModel: AiModelInfo): void
}
const props = withDefaults(defineProps<Props>(), {
  modelPlatform: '',
  modelName: () => emptyAiModel().modelName,
})
const emit = defineEmits<Emit>()
const appStore = useAppStore()

function renderLabel(option: SelectOption): VNodeChild {
  if (option.type === 'group')
    return option.label as string
  const val = option.value as string
  const llm = appStore.getLLMById(val)
  const modelPlatform = llm?.modelPlatform
  const isUnhealthy = llm?.healthStatus === 'UNHEALTHY'
  const children = [
    h('div', { class: 'flex items-center' }, {
      default: () => [
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
      ],
    }),
  ]
  if (isUnhealthy) {
    return h(NTooltip, { placement: 'right' }, {
      trigger: () => children[0],
      default: () => llm?.healthReason || 'Unavailable',
    })
  }
  return children[0]
}

const modelId = appStore.getLLMByPlatformAndName(props.modelPlatform, props.modelName)?.modelId || ''
const selectedModelId = ref<string>(modelId)
function handleSelect(modelId: string) {
  const aiModel = appStore.getLLMById(modelId)
  emit('llmSelected', aiModel || emptyAiModel())
}
</script>

<template>
  <NSelect
    v-model:value="selectedModelId" placement="top-start" trigger="click" :show-arrow="true"
    :options="appStore.llms" :render-label="renderLabel" @update:value="handleSelect"
  />
</template>
