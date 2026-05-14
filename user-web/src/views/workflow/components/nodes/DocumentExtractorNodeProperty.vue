<script setup lang="ts">
import { computed } from 'vue'
import NodePropertyInput from '../NodePropertyInput.vue'
import { useWfStore } from '@/store'
import { t } from '@/locales'
interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}

const props = defineProps<Props>()
const wfStore = useWfStore()

const userFileInput = computed(() => {
  return !!wfStore.getStartNode(props.workflow.uuid)?.inputConfig.user_inputs.find(input => input.type === 4)
})
</script>

<template>
  <div class="flex flex-col w-full">
    <div v-if="!userFileInput">
      {{ t('workflow.documentExtractorTip') }}
    </div>
    <NodePropertyInput
      v-else :workflow="workflow" :limit="1" :wf-node="wfNode" :only-show-start-node="true"
      :white-list-user-input-types="[4]"
    />
  </div>
</template>
