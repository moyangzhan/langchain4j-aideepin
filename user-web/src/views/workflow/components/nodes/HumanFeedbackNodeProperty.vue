<script setup lang="ts">
import { NInput } from 'naive-ui'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigHumanFeedback
</script>

<template>
  <div class="flex flex-col w-full">
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.inputTipLabel') }}
      </div>
      <div class="flex flex-col">
        <NInput v-model:value="nodeConfig.tip" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
  </div>
</template>
