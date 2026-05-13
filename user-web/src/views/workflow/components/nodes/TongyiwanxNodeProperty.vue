<script setup lang="ts">
import { NButton, NIcon, NInput, NInputNumber, NSelect, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import WfImageModelSelector from '../WfImageModelSelector.vue'
import ReferComment from '../ReferComment.vue'
import { TONGYI_WANX_SIZE_OPTIONS } from '@/utils/constant'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigTongyiwanx

function selected(modelName: string) {
  console.log('nodeConfig.modelName', nodeConfig.model_name, modelName)
  nodeConfig.model_name = modelName
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.model') }}
      </div>
      <div>
        <WfImageModelSelector
          :model-name="nodeConfig.model_name" platform="dashscope" :exclulde-model-names="['wanx-background-generation-v2']"
          @image-model-selected="selected"
        />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.promptLabel') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <div>{{ t('workflow.promptEmptyUsePrevious') }}</div>
        </NTooltip>
      </div>
      <div class="flex flex-col">
        <ReferComment />
        <NInput v-model:value="nodeConfig.prompt" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.imageSize') }}
      </div>
      <div>
        <NSelect v-model:value="nodeConfig.size" :options="TONGYI_WANX_SIZE_OPTIONS" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.randomSeed') }}
      </div>
      <div class="flex space-x-2 items-center">
        <NInputNumber v-model:value="nodeConfig.seed" :min="-1" :max="2147483647" class="grow" />
        <NButton type="primary" size="tiny" ghost @click="nodeConfig.seed = Math.floor(Math.random() * 2147483647)">
          {{ t('workflow.randomGenerate') }}
        </NButton>
      </div>
    </div>
  </div>
</template>
