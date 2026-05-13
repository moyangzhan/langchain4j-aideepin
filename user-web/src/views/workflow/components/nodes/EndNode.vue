<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'

const props = defineProps<NodeProps>()
const x = computed(() => `${Math.round(props.position.x)}px`)
const y = computed(() => `${Math.round(props.position.y)}px`)
console.log('x,y', x, y)
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="leading-6 bg-gray-100 mb-1 px-1 text-left text-xs line-clamp-3">
        {{ data.nodeConfig.result }}
      </div>
    </div>
  </div>
</template>
