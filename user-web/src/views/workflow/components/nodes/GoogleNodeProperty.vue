<script setup lang="ts">
import { NIcon, NInput, NInputNumber, NSelect, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import ReferComment from '../ReferComment.vue'
import { GOOGLE_COUNTRY_OPTIONS, GOOGLE_LANGUAGE_OPTIONS } from '@/utils/constant'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigGoogleSearch
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.queryContent') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <div>{{ t('workflow.queryEmptyUsePrevious') }}</div>
        </NTooltip>
      </div>
      <div class="flex flex-col">
        <ReferComment />
        <NInput v-model:value="nodeConfig.query" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.extractCount') }}
      </div>
      <div>
        <NInputNumber v-model:value="nodeConfig.top_n" :min="1" :max="30" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.countryAndRegion') }}
      </div>
      <div>
        <NSelect v-model:value="nodeConfig.country" :options="GOOGLE_COUNTRY_OPTIONS" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.language') }}
      </div>
      <div>
        <NSelect v-model:value="nodeConfig.language" :options="GOOGLE_LANGUAGE_OPTIONS" />
      </div>
    </div>
  </div>
</template>
