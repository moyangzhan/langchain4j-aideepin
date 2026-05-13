<script setup lang="ts">
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import { useWfStore } from '@/store'
import { t } from '@/locales'
import { getInputLabelFromParamName } from '@/utils/workflow-util'

interface WfProps {
  workflow: Workflow.WorkflowInfo
}
interface CombinedProps extends NodeProps, WfProps { }
defineProps<CombinedProps>()
const wfStore = useWfStore()
let yposition = 0
function startPosition() {
  yposition = 40
  return yposition
}
function inceaseYPosition() {
  yposition += 36
  return yposition
}
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <CommonNodeHeader :wf-node="data" :data-yposition="startPosition()" />
    <div clas="flex-1 flex-col">
      <div v-for="(wfCase, idx) in data.nodeConfig.cases" :key="wfCase.target_node_uuid" class="flex flex-col">
        <div class="h-8 leading-8 mt-1 px-1 bg-gray-200 rounded-md" :data-yposition="inceaseYPosition()">
          {{ t('workflow.branchCase') }}{{ idx + 1 }}
          <Handle :id="wfCase.uuid" type="source" :position="Position.Right" :style="`top: ${yposition}px`" />
        </div>
        <div class="flex flex-col items-center">
          <div
            v-for="(condition, cidx) in wfCase.conditions" :key="condition.uuid"
            class="flex w-full h-8 mt-1 p-1 bg-gray-100 rounded-md" :data-yposition="inceaseYPosition()"
          >
            <div class="max-w-24 h-6 leading-6 overflow-hidden text-gray-500 px-1 rounded-md text-xs">
              {{ getInputLabelFromParamName(workflow, condition.node_uuid, condition.node_param_name) }}
            </div>
            <div class="h-6 leading-6 overflow-hidden px-1 text-xs">
              {{ wfStore.getOperatorDesc(condition.operator) }}
            </div>
            <div v-show="condition.operator !== 'empty' && condition.operator !== 'not empty'" class="h-6 leading-6 flex-1 overflow-hidden text-gray-500 px-1 rounded-md text-xs">
              {{ condition.value }}
            </div>
            <div
              v-if="cidx !== wfCase.conditions.length - 1" class="absolute text-blue-400 text-xs"
              :style="`right:10px;top: ${yposition + 12}px`"
            >
              {{ wfCase.operator === 'and' ? t('workflow.conditionAnd') : t('workflow.conditionOr') }}
            </div>
          </div>
        </div>
      </div>
      <div class="h-8 leading-8 mt-1 px-1 bg-gray-200 rounded-md" :data-yposition="inceaseYPosition()">
        {{ t('workflow.fallbackCase') }}
        <Handle id="default_handle" type="source" :position="Position.Right" :style="`top:${yposition}px`" />
      </div>
    </div>
  </div>
</template>
