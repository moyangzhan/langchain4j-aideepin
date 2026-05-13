<script setup lang="ts">
import { NIcon, NIconWrapper, NInput, NRadio, NRadioGroup, NSlider, NTooltip } from 'naive-ui'
import { AnimalCat24Regular, QuestionCircle16Regular } from '@vicons/fluent'
import WfKnowledgeSelector from '../WfKnowledgeSelector.vue'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}

const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigKnowledgeRetrieval

function onScoreChange(value: number) {
  nodeConfig.score = value
}
function onTopNChange(value: number) {
  nodeConfig.top_n = value
}
function onStrictChange(value: boolean) {
  nodeConfig.is_strict = value
}
function onKnowledgeSelected(uuid: string, name: string) {
  nodeConfig.knowledge_base_uuid = uuid
  nodeConfig.knowledge_base_name = name
}
</script>

<template>
  <div class="flex flex-col w-full">
    <div class="mt-2">
      <div class="text-xl mb-1">
        {{ t('workflow.knowledgeBaseLabel') }}
      </div>
      <div>
        <WfKnowledgeSelector :knowledge-base-uuid="nodeConfig.knowledge_base_uuid" @selected="onKnowledgeSelected" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.recallCount') }} ({{ nodeConfig.top_n }})
      </div>
      <div class="px-2">
        <NSlider :value="nodeConfig.top_n" :step="1" :min="1" :max="30" :on-update:value="onTopNChange">
          <template #thumb>
            <NIconWrapper :size="24" :border-radius="12">
              <NIcon :size="18" :component="AnimalCat24Regular" />
            </NIconWrapper>
          </template>
        </NSlider>
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.minScore') }} ({{ nodeConfig.score }})
      </div>
      <div class="px-2">
        <NSlider :value="nodeConfig.score" :step="0.1" :min="0.1" :max="1" :on-update:value="onScoreChange">
          <template #thumb>
            <NIconWrapper :size="24" :border-radius="12">
              <NIcon :size="18" :component="AnimalCat24Regular" />
            </NIconWrapper>
          </template>
        </NSlider>
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.strictMode') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <div>{{ t('knowledgeBase.strictModeDescShort') }}</div>
          <div>{{ t('knowledgeBase.looseModeDescShort') }}</div>
        </NTooltip>
      </div>
      <div>
        <NRadioGroup :value="nodeConfig.is_strict" name="radiogroup" :on-update:value="onStrictChange">
          <NRadio key="strict_yes" :value="true">
            {{ t('common.yes') }}
          </NRadio>
          <NRadio key="strict_no" :value="false">
            {{ t('common.no') }}
          </NRadio>
        </NRadioGroup>
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.defaultReply') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <div>{{ t('workflow.noAnswerUseDefault') }}</div>
        </NTooltip>
      </div>
      <div>
        <NInput v-model:value="nodeConfig.default_response" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
  </div>
</template>
