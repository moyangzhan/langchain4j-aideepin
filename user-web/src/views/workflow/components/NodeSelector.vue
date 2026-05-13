<script setup lang="ts">
import { h, ref } from 'vue'
import { NSelect } from 'naive-ui'
import type { VNodeChild } from 'vue'
import type { SelectGroupOption, SelectOption } from 'naive-ui'
import { emptyWorkflowInfo } from '@/utils/functions'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { SvgIcon } from '@/components/common'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
  selected?: string
}
const props = withDefaults(defineProps<Props>(), {
  workflow: () => emptyWorkflowInfo(),
})

const emit = defineEmits<Emit>()

interface Emit {
  (e: 'nodeSelected', uuid: string): void
}

function renderDropdownLabel(option: SelectOption): VNodeChild {
  if (option.type === 'group')
    return option.label as string
  const componentName = (option.componentName as string) || ''
  return [
    h('div', { class: 'flex items-center' }, {
      default: () => [h(
        SvgIcon,
        {
          icon: getIconByComponentName(componentName),
          class: getIconClassByComponentName(componentName),
          imageSize: 20,
        },
      ),
      h(
        'div',
        {
          class: 'ml-1.5',
        },
        { default: () => option.label as string },
      ),
      ],
    }),
  ]
}

const selected = ref<string>(props.selected || '')
const options: Array<SelectOption | SelectGroupOption> = []
const nodes = props.workflow.nodes
for (let i = 0; i < nodes.length; i++) {
  const node = nodes[i]
  if (node.uuid === props.wfNode.uuid || node.wfComponent.name === 'Start')
    continue

  options.push({
    label: node.title,
    value: node.uuid,
    componentName: node.wfComponent.name,
  })
}

// if(options.length > 0) {
//   selected.value = options[0].key as string
// }

function handleSelect(value: string) {
  console.log('node selected', value)
  emit('nodeSelected', value)
}
</script>

<template>
  <NSelect
    v-model:value="selected" placement="top-start" trigger="click" :show-arrow="true" :render-label="renderDropdownLabel" :options="options"
    :consistent-menu-width="false" @update:value="handleSelect"
  />
</template>
