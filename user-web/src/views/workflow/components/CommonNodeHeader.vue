<script setup lang="ts">
import { h } from 'vue'
import { NDropdown } from 'naive-ui'
import { SvgIcon } from '@/components/common'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { useWfStore } from '@/store'
import { t } from '@/locales'

const props = defineProps<Props>()
const options = [
  {
    label: t('common.delete'),
    key: 'delete',
    icon: renderIcon('ri:delete-bin-line'),
  },
]
interface Props {
  wfNode: Workflow.WorkflowNode
}
const wfStore = useWfStore()

function renderIcon(icon: string) {
  return () => {
    return h(
      SvgIcon,
      {
        icon,
        class: 'text-base cursor-pointer',
      })
  }
}

function handleSelect(key: string | number) {
  if (key === 'delete') {
    console.log('delete node', key)
    wfStore.deleteNode(props.wfNode.workflowUuid, props.wfNode.uuid)
  }
}
</script>

<template>
  <div
    class="w-full flex border-b divide-gray-400 pb-3 mb-3 font-bold text-base text-center items-center justify-center"
  >
    <div class="w-6 mr-2">
      <SvgIcon
        class="text-xl" :class="getIconClassByComponentName(wfNode.wfComponent.name)"
        :icon="getIconByComponentName(wfNode.wfComponent.name)"
      />
    </div>
    <div class="flex-1 max-h-6 overflow-hidden text-nowrap">
      {{ wfNode.title }}
    </div>
    <div class="w-6 ml-2">
      <NDropdown v-if="wfNode.wfComponent.name !== 'Start'" :options="options" @select="handleSelect">
        <SvgIcon class="cursor-pointer" icon="ri:more-fill" />
      </NDropdown>
    </div>
  </div>
</template>
