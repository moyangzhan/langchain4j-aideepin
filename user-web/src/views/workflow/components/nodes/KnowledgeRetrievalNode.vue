<script setup lang="ts">
import { NIcon } from 'naive-ui'
import { Cloud32Regular, LockClosed32Regular } from '@vicons/fluent'
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
    <div clas="flex-1 flex-col">
      <div class="content_line flex items-center">
        <NIcon v-if="data.nodeConfig.isPublic" :component="Cloud32Regular" class="mx-1.5" />
        <NIcon v-if="!data.nodeConfig.isPublic" :component="LockClosed32Regular" class="mx-1.5" />
        {{ data.nodeConfig.knowledge_base_name }}
      </div>
    </div>
  </div>
</template>
