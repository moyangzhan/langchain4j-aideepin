<script setup lang="ts">
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'

defineProps<NodeProps>()
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <Handle type="source" :position="Position.Right" />
    <CommonNodeHeader :wf-node="data" />
    <div class="flex-1 flex-col">
      <div class="content_line flex items-center">
        <span class="iconify mx-1.5" data-icon="carbon:bot" />
        {{ (data.nodeConfig as Workflow.NodeConfigAgent).character_uuid ? 'Agent' : '' }}
      </div>
    </div>
  </div>
</template>
