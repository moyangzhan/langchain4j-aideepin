<script setup lang="ts">
import { NIcon, NInput, NSelect, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import ReferComment from '../ReferComment.vue'
import { OPENAI_IMAGE_QUALITY_OPTIONS, OPENAI_IMAGE_SIZE_OPTIONS } from '@/utils/constant'
import { t } from '@/locales'
interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigOpenAiImage
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
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
        <NSelect v-model:value="nodeConfig.size" :options="OPENAI_IMAGE_SIZE_OPTIONS" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.imageQuality') }}
      </div>
      <div>
        <NSelect v-model:value="nodeConfig.quality" :options="OPENAI_IMAGE_QUALITY_OPTIONS" />
      </div>
    </div>
  </div>
</template>
