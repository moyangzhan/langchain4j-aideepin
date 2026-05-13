<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { useAppStore } from '@/store'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'

const props = defineProps<NodeProps>()
const appStore = useAppStore()
const modelPlatform = computed(() => {
  return appStore.getLLMByName(props.data.nodeConfig.model_name)?.modelPlatform || ''
})
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <Handle type="source" :position="Position.Right" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="content_line flex items-center">
        <AvatarComponent :name="modelPlatform" :image-size="20" class="mx-1.5" />
        {{ data.nodeConfig.model_name }}
      </div>
    </div>
  </div>
</template>
