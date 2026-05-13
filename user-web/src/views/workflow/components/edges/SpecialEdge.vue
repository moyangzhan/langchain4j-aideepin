<script setup lang="ts">
import { computed } from 'vue'
import { BaseEdge, EdgeLabelRenderer, getBezierPath } from '@vue-flow/core'
import { type EdgeProps } from '@vue-flow/core'
import { SvgIcon } from '@/components/common'
import { useWfStore } from '@/store'

const props = defineProps<EdgeProps>()
const wfStore = useWfStore()
const path = computed(() => getBezierPath(props))

function removeEdge() {
  console.log('removeEdge', props.id)
  const edge = props.data as Workflow.WorkflowEdge
  wfStore.deleteEdge(edge.workflowUuid, props.id)
}
</script>

<script lang="ts">
export default {
  inheritAttrs: false,
}
</script>

<template>
  <!-- You can use the `BaseEdge` component to create your own custom edge more easily -->
  <BaseEdge :path="path[0]" />

  <!-- Use the `EdgeLabelRenderer` to escape the SVG world of edges and render your own custom label in a `<div>` ctx -->
  <EdgeLabelRenderer>
    <div
      :style="{
        pointerEvents: 'all',
        position: 'absolute',
        transform: `translate(-50%, -50%) translate(${path[1]}px,${path[2]}px)`,
      }" class="nodrag nopan"
    >
      <SvgIcon
        v-show="selected" icon="typcn:delete-outline" class="text-blue-800 text-xl cursor-pointer"
        @click="removeEdge"
      />
    </div>
  </EdgeLabelRenderer>
</template>
