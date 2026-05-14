<script setup lang="ts">
import { v4 as uuidv4 } from 'uuid'
import { NAvatar, NButton, NCollapse, NCollapseItem, NInput, NTag, useMessage } from 'naive-ui'
import NodeSelector from '../NodeSelector.vue'
import WfVariableSelector from '../WfVariableSelector.vue'
import OperatorSelector from '../OperatorSelector.vue'
import shouzhitishi from '@/assets/shouzhitishi.svg'
import { createNewEdge, deleteEdgesBySourceHandle, updateEdgeBySourceHandle } from '@/utils/workflow-util'
import { t } from '@/locales'
import { SvgIcon } from '@/components/common'
import { useWfStore } from '@/store'


interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
  uiWorkflow: Workflow.UIWorkflow
}

const props = defineProps<Props>()
const wfStore = useWfStore()
const ms = useMessage()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigSwitcher

function onConditionSelected(wfInput: Workflow.NodeConfigSwitcherCaseCondition, nodeUuidParamName: string[]) {
  wfInput.node_uuid = nodeUuidParamName[0]
  wfInput.node_param_name = nodeUuidParamName[1]
}

function onCaseNextNodeSelected(wfCase: Workflow.NodeConfigSwitcherCase, nodeUuid: string) {
  wfCase.target_node_uuid = nodeUuid
  const params = {
    workflow: props.workflow,
    uiWorkflow: props.uiWorkflow,
    source: props.wfNode.uuid,
    sourceHandle: wfCase.uuid,
    target: nodeUuid,
  }
  updateEdgeBySourceHandle(params)
}

function onDefaultCaseTarget(nodeUuid: string) {
  nodeConfig.default_target_node_uuid = nodeUuid
}

function onAddCondition(wfCase: Workflow.NodeConfigSwitcherCase) {
  const startNode = wfStore.getStartNode(props.workflow.uuid)
  const nodeParamaName = startNode?.inputConfig.user_inputs[0].name
  wfCase.conditions.push({
    uuid: uuidv4().replace(/-/g, ''),
    node_uuid: startNode?.uuid,
    node_param_name: nodeParamaName,
    operator: '=',
    value: '',
  })
}

function onDelCondition(wfCase: Workflow.NodeConfigSwitcherCase, wfCondition: Workflow.NodeConfigSwitcherCaseCondition) {
  if (wfCase.conditions.length === 1) {
    ms.warning(t('workflow.keepAtLeastOneCondition'))
    return
  }
  const idx = wfCase.conditions.findIndex(item => item.uuid === wfCondition.uuid)
  if (idx >= 0)
    wfCase.conditions.splice(idx, 1)
}

function onAddCase() {
  const uuid = uuidv4().replace(/-/g, '')
  const wfCase = {
    uuid,
    operator: 'and',
    target_node_uuid: '',
    conditions: [],
  }
  nodeConfig.cases.push(wfCase)
  onAddCondition(wfCase)

  // 创建一条新的边
  const params = {
    workflow: props.workflow,
    uiWorkflow: props.uiWorkflow,
    source: props.wfNode.uuid,
    sourceHandle: uuid,
    target: '',
  }
  createNewEdge(params)
}

function onDelCase(nodeCase: Workflow.NodeConfigSwitcherCase) {
  if (nodeConfig.cases.length === 1) {
    ms.warning(t('workflow.keepAtLeastOneBranch'))
    return
  }
  const idx = nodeConfig.cases.findIndex(item => item.uuid === nodeCase.uuid)
  if (idx >= 0) {
    const wfCase = nodeConfig.cases[idx]
    nodeConfig.cases.splice(idx, 1)
    deleteEdgesBySourceHandle(props.workflow, props.uiWorkflow, props.wfNode.uuid, wfCase.uuid)
  }
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NCollapse :default-expanded-names="['0']">
      <NCollapseItem
        v-for="(wfCase, idx) in nodeConfig.cases" :key="wfCase.uuid" :name="`${idx}`"
        class="border border-gray-200 rounded-md m-2"
      >
        <template #header>
          <div class="pl-1">
            {{ t('workflow.branchCaseIndex', { index: idx + 1 }) }}
          </div>
        </template>
        <template #header-extra>
          <div v-show="nodeConfig.cases.length > 1" class="p-2 cursor-pointer" @click="onDelCase(wfCase)">
            X
          </div>
        </template>
        <div class="flex flex-col w-full bg-gray-100 px-3 pb-3 pt-3">
          <div class="flex">
            <div v-show="wfCase.conditions.length > 1" class="flex items-center justify-center">
              <div class="flex-1" />
              <NTag
                size="small" style="cursor: pointer;" class="rounded text-blue-400"
                type="info" round @click="wfCase.operator = wfCase.operator === 'and' ? 'or' : 'and'"
              >
                {{ wfCase.operator === 'and' ? t('workflow.conditionAnd') : t('workflow.conditionOr') }}
                <template #avatar>
                  <SvgIcon icon="mage:reload-reverse" />
                </template>
              </NTag>
              <div class="h-full py-3">
                <div class="h-full w-2 border border-gray-200 rounded-l-[8px] border-r-0 mr-1" />
              </div>
            </div>
            <div class="flex flex-col flex-1">
              <div v-for="condition in wfCase.conditions" :key="condition.uuid" class="w-full mb-2 flex">
                <WfVariableSelector
                  :workflow="workflow" :wf-node="wfNode" :wf-ref-var="condition"
                  :exclude-nodes="[wfNode.wfComponent.name]"
                  class="flex-1 mr-1 h-full max-w-[150px]" @variable-selected="onConditionSelected(condition, $event)"
                />
                <OperatorSelector :selected="condition.operator" class="mr-1 h-full max-w-[120px]" @operator-selected="(op) => condition.operator = op" />
                <NInput v-show="condition.operator !== 'empty' && condition.operator !== 'not empty'" v-model:value="condition.value" class="flex-1 min-w-16" />
                <SvgIcon
                  v-show="wfCase.conditions.length > 1" class="text-base ml-0.5 h-full cursor-pointer"
                  icon="ep:remove" @click="onDelCondition(wfCase, condition)"
                />
              </div>
            </div>
          </div>
          <div class="my-3 border border-gray-200 rounded-md p-3">
            <div class="text-sm bg-indigo-100 rounded-md p-2 my-2 w-full flex">
              <div v-if="wfCase.operator === 'and'">
                {{ t('workflow.andConditionTip') }}
              </div>
              <div v-else>
                {{ t('workflow.orConditionTip') }}
              </div>
              <NAvatar
                object-fit="contain" round :size="18" :src="shouzhitishi"
                :style="{ backgroundColor: 'transparent' }"
              />
            </div>
            <NodeSelector
              :workflow="props.workflow" :wf-node="props.wfNode" :selected="wfCase.target_node_uuid"
              @node-selected="onCaseNextNodeSelected(wfCase, $event)"
            />
          </div>
          <NButton class="mt-2" dashed @click="onAddCondition(wfCase)">
            {{ t('workflow.newCondition') }}
          </NButton>
        </div>
      </NCollapseItem>
    </NCollapse>
    <br>
    <NCollapse :default-expanded-names="['default']">
      <NCollapseItem name="default" class="border border-gray-200 rounded-md m-2">
        <template #header>
          <div class="pl-1">
            {{ t('workflow.fallbackCase') }}
          </div>
        </template>
        <div class="flex flex-col w-full bg-gray-100 px-3 pb-3">
          <div class="text-sm bg-blue-100 rounded-md p-2 my-2 w-full flex">
            <div>
              {{ t('workflow.fallbackTip', { count: nodeConfig.cases.length }) }}
            </div>
            <NAvatar
              object-fit="contain" round :size="18" :src="shouzhitishi"
              :style="{ backgroundColor: 'transparent' }"
            />
          </div>
          <NodeSelector
            class="flex-1" :workflow="props.workflow" :wf-node="props.wfNode"
            :selected="nodeConfig.default_target_node_uuid" @node-selected="onDefaultCaseTarget($event)"
          />
        </div>
      </NCollapseItem>
    </NCollapse>
    <br>
    <NButton class="mt-2" dashed @click="onAddCase">
      <span class="font-bold">{{ t('workflow.newCase') }}</span>
    </NButton>
  </div>
</template>
