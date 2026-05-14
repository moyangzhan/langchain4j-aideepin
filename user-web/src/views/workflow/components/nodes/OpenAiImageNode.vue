<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { OPENAI_IMAGE_QUALITY_OPTIONS, OPENAI_IMAGE_SIZE_OPTIONS } from '@/utils/constant'
import { t } from '@/locales'
const props = defineProps<NodeProps>()
const sizeLabel = computed(() => {
  return OPENAI_IMAGE_SIZE_OPTIONS.find(item => item.value === props.data.nodeConfig.size)?.label || ''
})
const qualityLabel = computed(() => {
  return OPENAI_IMAGE_QUALITY_OPTIONS.find(item => item.value === props.data.nodeConfig.quality)?.label || ''
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
        {{ t('workflow.imageQualityLabel') }}{{ qualityLabel }}
      </div>
    </div>
  </div>
</template>
