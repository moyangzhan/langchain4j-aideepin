<script setup lang="ts">
import { NTag } from 'naive-ui'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { t } from '@/locales'

defineProps<NodeProps>()
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="source" :position="Position.Right" />
    <Handle type="target" :position="Position.Left" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="content_line flex items-center">
        <NTag size="small" round>
          {{ data.nodeConfig.method }}
        </NTag>
        {{ data.nodeConfig.url }}
      </div>
      <div class="content_line flex items-center">
        Content-Type： {{ data.nodeConfig.content_type }}
      </div>
      <div class="content_line flex items-center">
        {{ t('workflow.timeoutLabel') }}{{ data.nodeConfig.timeout }} s
      </div>
      <div class="content_line flex items-center">
        {{ t('workflow.retryCountLabel') }}{{ data.nodeConfig.retry_times }}
      </div>
    </div>
  </div>
</template>
