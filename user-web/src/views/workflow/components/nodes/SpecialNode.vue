<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'

const props = defineProps<NodeProps>()
const x = computed(() => `${Math.round(props.position.x)}px`)
const y = computed(() => `${Math.round(props.position.y)}px`)
console.log('x,y', x, y)
</script>

<template>
  <div>
    <div>{{ data.label }}</div>
    <div style="position: relative; padding: 10px">
      <Handle type="source" :position="Position.Right" />
      <Handle type="target" :position="Position.Left" />
    </div>
  </div>
</template>
