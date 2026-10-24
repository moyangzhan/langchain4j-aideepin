<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { IMAGE_GENERATOR_SIZE_OPTIONS } from '@/utils/constant'
import { useAppStore } from '@/store'
import { t } from '@/locales'
const props = defineProps<NodeProps>()
const appStore = useAppStore()
// 尺寸标签优先按所选模型的 properties.sizes 查,查不到再回退默认档位
// Resolve the size label from the selected model's properties.sizes first, then fall back to defaults
const sizeLabel = computed(() => {
  const modelName = props.data.nodeConfig.model_name
  const model = modelName ? appStore.getImageModelByName(modelName) : null
  const sizes = (model?.properties as ImageModelProperties | undefined)?.sizes
  return sizes?.find(item => item.value === props.data.nodeConfig.size)?.label
    || IMAGE_GENERATOR_SIZE_OPTIONS.find(item => item.value === props.data.nodeConfig.size)?.label
    || props.data.nodeConfig.size
    || ''
})
// 摘要行的种子/质量按模型声明条件显示(与属性面板一致)
// Seed/quality summary lines are conditional on the model's declarations (same as the property panel)
const modelProps = computed(() => {
  const modelName = props.data.nodeConfig.model_name
  const model = modelName ? appStore.getImageModelByName(modelName) : null
  return model?.properties as ImageModelProperties | undefined
})
const supportsSeed = computed(() => modelProps.value?.is_support_seed ?? false)
const showQuality = computed(() => (modelProps.value?.qualities?.length ?? 0) > 0)
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
      <div v-if="showQuality" class="content_line flex items-center pl-2">
        {{ t('workflow.imageQualityLabel') }}{{ data.nodeConfig.quality }}
      </div>
      <div v-if="supportsSeed" class="content_line flex items-center pl-2">
        {{ t('workflow.randomSeedLabel') }}{{ data.nodeConfig.seed }}
      </div>
    </div>
  </div>
</template>
