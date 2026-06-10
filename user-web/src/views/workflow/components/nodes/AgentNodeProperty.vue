<script setup lang="ts">
import { NIcon, NInput, NSelect, NSwitch, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import ReferComment from '../ReferComment.vue'
import WfLLMSelector from '../WfLLMSelector.vue'
import { t } from '@/locales'
import { onMounted, ref } from 'vue'
import api from '@/api'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}

const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigAgent

const characterOptions = ref<{ label: string, value: string }[]>([])

onMounted(async () => {
  try {
    const { data } = await api.fetchCharacters<{ data: { uuid: string; title: string }[] }>()
    if (data && Array.isArray(data)) {
      characterOptions.value = data.map((c: any) => ({ label: c.title, value: c.uuid }))
    }
  } catch (e) {
    console.error('Failed to load characters', e)
  }
})

function llmSelected(aiModel: AiModelInfo) {
  nodeConfig.model_name = aiModel.modelName
  nodeConfig.model_platform = aiModel.modelPlatform
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />

    <!-- Character 选择器 -->
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.characterLabel') || 'Character' }}
      </div>
      <NSelect
        v-model:value="nodeConfig.character_uuid"
        :options="characterOptions"
        filterable
        :placeholder="t('workflow.selectCharacter') || 'Select a Character'"
      />
    </div>

    <!-- 模型选择器（可选） -->
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('common.model') }}
      </div>
      <WfLLMSelector :model-platform="nodeConfig.model_platform" :model-name="nodeConfig.model_name" @llm-selected="llmSelected" />
    </div>

    <!-- Prompt 模板 -->
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('common.promptLabel') || 'Prompt' }}
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

    <!-- 功能开关 -->
    <div class="mt-6 flex flex-col gap-3">
      <div class="text-xl mb-1">
        {{ t('workflow.capabilities') || 'Capabilities' }}
      </div>
      <div class="flex items-center gap-2">
        <NSwitch v-model:value="nodeConfig.enable_rag" />
        <span>RAG</span>
      </div>
      <div class="flex items-center gap-2">
        <NSwitch v-model:value="nodeConfig.enable_mcp" />
        <span>MCP {{ t('workflow.toolsLabel') || 'Tools' }}</span>
      </div>
      <div class="flex items-center gap-2">
        <NSwitch v-model:value="nodeConfig.enable_web_search" />
        <span>{{ t('workflow.webSearch') || 'Web Search' }}</span>
      </div>
    </div>
  </div>
</template>
