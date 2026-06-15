<script setup lang="ts">
import { h, ref, computed, onMounted } from 'vue'
import { NSelect, NTooltip } from 'naive-ui'
import type { SelectOption } from 'naive-ui'
import type { VNodeChild } from 'vue'
import { useAppStore } from '@/store'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import { emptyAiModel } from '@/utils/functions'

interface Props {
  modelPlatform?: string
  modelName?: string
  /**
   * Restrict the dropdown to a single backend model type ('text' | 'vision' | etc.).
   * Workflow LLM nodes (Answer / Agent / Classifier / extractors) do text tasks, so the
   * default is 'text' — note that many text models do support image input via inputTypes,
   * so filtering by type (purpose) instead of inputTypes (capability) is the correct lens.
   * Pass an empty string to disable filtering.
   */
  modelType?: string
}
interface Emit {
  (e: 'llmSelected', aiModel: AiModelInfo): void
}
const props = withDefaults(defineProps<Props>(), {
  modelPlatform: '',
  modelName: () => emptyAiModel().modelName,
  modelType: 'text',
})
const emit = defineEmits<Emit>()
const appStore = useAppStore()

// appStore.llms holds chat-purpose models (text + vision); image / embedding / tts / asr are
// already segregated by separate APIs. Filter by `type` so we keep purpose-text models even
// when they happen to accept image input.
const options = computed(() => {
  if (!props.modelType)
    return appStore.llms
  return appStore.llms.filter(item => item.type === props.modelType)
})

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

// Default-select the first enabled & healthy model when none is selected, so a freshly-created
// node shows a concrete model instead of a blank dropdown. Notifies the parent via llmSelected
// so it can persist the choice. Skipped if the list is empty (e.g. models still loading).
onMounted(() => {
  if (selectedModelId.value)
    return
  const first = options.value.find(item => item.enable && item.healthStatus !== 'UNHEALTHY')
  if (first) {
    selectedModelId.value = first.modelId
    handleSelect(first.modelId)
  }
})
</script>

<template>
  <NSelect
    v-model:value="selectedModelId" placement="top-start" trigger="click" :show-arrow="true"
    :options="options" :render-label="renderLabel" @update:value="handleSelect"
  />
</template>
