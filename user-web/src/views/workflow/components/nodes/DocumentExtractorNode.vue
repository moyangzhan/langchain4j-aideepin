<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { useWfStore } from '@/store'

interface WfProps {
  workflow: Workflow.WorkflowInfo
}
interface CombinedProps extends NodeProps, WfProps { }
const props = defineProps<CombinedProps>()
const wfStore = useWfStore()
const userFilesInputTitle = computed(() => {
  return wfStore.getStartNode(props.workflow.uuid)?.inputConfig.user_inputs.find(input => input.type === 4)?.title || ''
})
const firstFileInput = computed(() => {
  if (props.data.inputConfig.ref_inputs.length === 0)
    return null
  else
    return props.data.inputConfig.ref_inputs[0]
})
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="source" :position="Position.Right" />
    <Handle type="target" :position="Position.Left" />
    <CommonNodeHeader :wf-node="data" />
    <div clas="flex-1 flex-col">
      <div class="content_line flex justify-items-start">
        <div class="w-24 overflow-hidden mr-1">
          {{ firstFileInput?.name }}
        </div>
        <div class="flex-1 overflow-hidden">
          {{ userFilesInputTitle }}
        </div>
      </div>
    </div>
  </div>
</template>
