<script setup lang="ts">
import { h, nextTick, ref } from 'vue'
import { NSelect } from 'naive-ui'
import type { VNodeChild } from 'vue'
import type { SelectGroupOption, SelectOption } from 'naive-ui'
import { emptyWorkflowInfo } from '@/utils/functions'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { SvgIcon } from '@/components/common'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
  wfRefVar: Workflow.NodeIORefDinition
  excludeNodes: string[]
  whiteListComponents?: string[]
  whiteListUserInputTypes?: number[]
}
const props = withDefaults(defineProps<Props>(), {
  workflow: () => emptyWorkflowInfo(),
  whiteListComponents: () => [],
})

const emit = defineEmits<Emit>()

interface Emit {
  (e: 'variableSelected', nodeUuidParanmName: string[]): void
}

const selectedVar = ref<string>('')

const userInputGroup = {
  type: 'group',
  label: t('workflow.userInput'),
  key: 'userInput',
  children: [] as Array<{ label: string; value: string }>,
}

const componentOutputGroup = {
  type: 'group',
  label: t('workflow.nodeOutput'),
  key: 'componentOutput',
  children: [] as Array<{ label: string; value: string }>,
}

function renderDropdownLabel(option: SelectOption): VNodeChild {
  if (option.type === 'group')
    return option.label as string
  const val = option.value as string
  const nodeUuid = val.split('::')[0]
  const componentName = props.workflow.nodes.find(item => item.uuid === nodeUuid)?.wfComponent.name || ''
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

const options: Array<SelectOption | SelectGroupOption> = [userInputGroup, componentOutputGroup]
const nodes = props.workflow.nodes
for (let i = 0; i < nodes.length; i++) {
  const node = nodes[i]
  if (props.excludeNodes.includes(node.uuid) || node.wfComponent.name === 'End')
    continue
  if (props.whiteListComponents.length > 0 && !props.whiteListComponents.includes(node.wfComponent.name))
    continue
  const inputConfig = node.inputConfig
  if (node.wfComponent.name === 'Start') {
    for (let j = 0; j < inputConfig.user_inputs.length; j++) {
      const userInput = inputConfig.user_inputs[j]
      if (props.whiteListUserInputTypes && props.whiteListUserInputTypes.length > 0 && !props.whiteListUserInputTypes.includes(userInput.type))
        continue
      userInputGroup.children.push({
        label: userInput.title,
        value: `${node.uuid}::${userInput.name}`,
      })
    }
  } else {
    componentOutputGroup.children.push({
      label: node.title,
      value: `${node.uuid}::output`,
    })
  }
}
console.log('options', options)
nextTick(() => {
  console.log('selectedVar', selectedVar)
  selectedVar.value = `${props.wfRefVar.node_uuid}::${props.wfRefVar.node_param_name}`
  console.log('selectedVar2', selectedVar)
})

function handleSelect(value: string) {
  const vs = value.split('::')
  emit('variableSelected', vs)
}
</script>

<template>
  <NSelect
    v-model:value="selectedVar" placement="top-start" trigger="click" :show-arrow="true"
    :render-label="renderDropdownLabel" :options="options" :consistent-menu-width="false"
    @update:value="handleSelect"
  />
</template>
