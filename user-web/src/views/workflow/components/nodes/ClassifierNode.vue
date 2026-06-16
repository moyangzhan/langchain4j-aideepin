<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { useAppStore } from '@/store'
import AvatarComponent from '@/views/chat/components/Message/Avatar.vue'

const props = defineProps<NodeProps>()
const appStore = useAppStore()
// Resolve by platform+name when both are present (avoids cross-platform name
// collisions); fall back to name-only for legacy nodes saved before the
// platform field existed.
const currentLLM = computed(() => {
  const cfg = props.data.nodeConfig as Workflow.NodeConfigClassifier
  return appStore.getLLMByPlatformAndName(cfg.model_platform || '', cfg.model_name || '')
})
const modelPlatform = computed(() => currentLLM.value?.modelPlatform || '')
const modelLabel = computed(() => currentLLM.value?.modelTitle || currentLLM.value?.modelName || (props.data.nodeConfig as Workflow.NodeConfigClassifier).model_name || '')
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="content_line flex items-center">
        <AvatarComponent :name="modelPlatform" :image-size="20" class="mx-1.5" />
        <span class="truncate" :title="modelLabel">{{ modelLabel }}</span>
      </div>
      <div v-for="(category, idx) in data.nodeConfig.categories" :key="category.category_uuid" class="content_line">
        {{ category.category_name }}
        <Handle
          :id="category.category_uuid" type="source" :position="Position.Right"
          :style="{ top: `${130 + (idx as number) * 50}px` }"
        />
      </div>
    </div>
  </div>
</template>
