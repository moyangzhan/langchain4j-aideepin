<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { TONGYI_WANX_SIZE_OPTIONS } from '@/utils/constant'
import { useAppStore } from '@/store'
import { t } from '@/locales'
const props = defineProps<NodeProps>()
const appStore = useAppStore()
// 尺寸标签优先按所选模型的 properties.sizes 查,查不到再回退默认档位
// Resolve the size label from the selected model's properties.sizes first, then fall back to defaults
const sizeLabel = computed(() => {
  const modelName = props.data.nodeConfig.model_name
  const model = modelName ? appStore.getImageModelByName(modelName) : null
  const sizes = (model?.properties as { sizes?: { value: string, label: string }[] } | undefined)?.sizes
  return sizes?.find(item => item.value === props.data.nodeConfig.size)?.label
    || TONGYI_WANX_SIZE_OPTIONS.find(item => item.value === props.data.nodeConfig.size)?.label
    || props.data.nodeConfig.size
    || ''
})
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <Handle type="source" :position="Position.Right" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="content_line flex items-center pl-2">
        {{ t('workflow.imageSizeLabel') }}{{ sizeLabel }}
      </div>
      <div class="content_line flex items-center pl-2">
        {{ t('workflow.randomSeedLabel') }}{{ data.nodeConfig.seed }}
      </div>
    </div>
  </div>
</template>
