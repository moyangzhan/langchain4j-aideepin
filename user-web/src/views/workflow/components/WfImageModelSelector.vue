<script setup lang="ts">
import { h, ref } from 'vue'
import { NSelect } from 'naive-ui'
import type { SelectOption } from 'naive-ui'
import type { VNodeChild } from 'vue'
import { useAppStore } from '@/store'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'
import { emptyAiModel } from '@/utils/functions'

interface Props {
  modelName: string
  platform: string
  excluldeModelNames?: string[]
}
interface Emit {
  (e: 'imageModelSelected', modelName: string): void
}
const props = withDefaults(defineProps<Props>(), {
  modelName: () => emptyAiModel().modelName,
})
const emit = defineEmits<Emit>()
const appStore = useAppStore()
let options = appStore.imageModels
if (props.platform) {
  options = options.filter((item) => {
    if (props.excluldeModelNames && props.excluldeModelNames.includes(item.modelName))
      return false

    return item.modelPlatform === props.platform
  })
}
function renderLabel(option: SelectOption): VNodeChild {
  if (option.type === 'group')
    return option.label as string
  const val = option.value as string
  const modelPlatform = appStore.getImageModelByName(val)?.modelPlatform
  return [
    h('div', { class: 'flex items-center' }, {
      default: () => [
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
            class: 'ml-1.5',
          },
          { default: () => option.label as string },
        ),
      ],
    }),
  ]
}

const selectedModelName = ref<string>(props.modelName)
console.log('selectedModelName', selectedModelName.value)
if (!selectedModelName.value && options.length > 0)
  selectedModelName.value = options[0].value

function handleSelect(modelName: string) {
  emit('imageModelSelected', modelName)
}
</script>

<template>
  <NSelect
    v-model:value="selectedModelName" placement="top-start" trigger="click" :show-arrow="true" :options="options"
    :render-label="renderLabel" @update:value="handleSelect"
  />
</template>
