<script setup lang="ts">
import { NIcon, NInput, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import ReferComment from '../ReferComment.vue'
import WfLLMSelector from '../WfLLMSelector.vue'
import { t } from '@/locales'
interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}

const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigAnswer

function llmSelected(aiModel: AiModelInfo) {
  console.log('nodeConfig.modelName', nodeConfig.model_name, aiModel.modelName)
  nodeConfig.model_name = aiModel.modelName
  nodeConfig.model_platform = aiModel.modelPlatform
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('common.model') }}
      </div>
      <div>
        <WfLLMSelector :model-platform="nodeConfig.model_platform" :model-name="nodeConfig.model_name" @llm-selected="llmSelected" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('common.prompt') }}
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
  </div>
</template>
