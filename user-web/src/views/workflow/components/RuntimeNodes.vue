<script lang="ts" setup>
import { NImage, NImageGroup } from 'naive-ui'
import { SvgIcon } from '@/components/common'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { useAuthStore } from '@/store'
import { formatDuration } from '@/utils/format'
import { t } from '@/locales'
import { getRealFileUrl } from '@/utils/functions'
import TextComponent from '@/views/chat/components/Message/Text.vue'

interface Props {
  nodes: Workflow.WfRuntimeNode[]
  workflow: Workflow.WorkflowInfo
  errorMsg: string
}
defineProps<Props>()
const authStore = useAuthStore()
</script>

<template>
  <div>
    <div v-if="errorMsg" class="py-2 text-red-500">
      {{ t('workflow.errorLabel') }}{{ errorMsg }}
    </div>
    <div v-else-if="nodes.length === 0" class="text-center py-2 text-neutral-400">
      {{ t('common.noContent') }}
    </div>
    <div
      v-for="node in nodes" :key="node.uuid"
      class="flex flex-col space-y-2 border border-gray-200 p-2 m-2 rounded-md" :title="node.nodeTitle"
      :name="node.uuid"
    >
      <div class="flex items-center space-x-1 bg-gray-100 px-2 py-1 rounded-md">
        <SvgIcon
          v-if="node.wfComponent" class="text-base" :class="getIconClassByComponentName(node.wfComponent.name)"
          :icon="getIconByComponentName(node.wfComponent.name)"
        />
        <div class="text-base">
          {{ node.nodeTitle || t('workflow.nodeTitleNotFound') }}
        </div>
      </div>
      <div v-if="node.duration || node.metadata" class="flex flex-wrap gap-1.5 px-2 py-1 text-xs text-gray-500">
        <span v-if="formatDuration(node.duration)" class="metric-chip">
          ⏱ {{ formatDuration(node.duration) }}
        </span>
        <!-- LLM / Agent: token + model -->
        <template v-if="node.metadata?.type === 'llm' || node.metadata?.type === 'agent'">
          <span v-if="node.metadata.inputTokens != null" class="metric-chip">
            📥 {{ node.metadata.inputTokens }} tokens
          </span>
          <span v-if="node.metadata.outputTokens != null" class="metric-chip">
            📤 {{ node.metadata.outputTokens }} tokens
          </span>
          <span v-if="node.metadata.modelName" class="metric-chip">
            🤖 {{ node.metadata.modelName }}
          </span>
        </template>
        <!-- Agent: extra RAG retrieval count -->
        <template v-if="node.metadata?.type === 'agent'">
          <span v-if="node.metadata.retrievalCount != null" class="metric-chip">
            📚 {{ node.metadata.retrievalCount }} chunks
          </span>
        </template>
        <!-- HTTP request -->
        <template v-if="node.metadata?.type === 'http_request'">
          <span
            v-if="node.metadata.httpStatusCode"
            class="px-1.5 py-0.5 rounded"
            :class="node.metadata.httpStatusCode === 200 ? 'bg-green-50 text-green-600' : 'bg-red-50 text-red-500'"
          >
            HTTP {{ node.metadata.httpStatusCode }}
          </span>
        </template>
        <!-- Search -->
        <template v-if="node.metadata?.type === 'search'">
          <span v-if="node.metadata.searchResultCount != null" class="metric-chip">
            🔍 {{ node.metadata.searchResultCount }} results
          </span>
        </template>
        <!-- Knowledge retrieval -->
        <template v-if="node.metadata?.type === 'knowledge_retrieval'">
          <span v-if="node.metadata.retrievalCount != null" class="metric-chip">
            📚 {{ node.metadata.retrievalCount }} chunks
          </span>
        </template>
        <!-- Image generation -->
        <template v-if="node.metadata?.type === 'image'">
          <span v-if="node.metadata.imageModelName" class="metric-chip">
            🎨 {{ node.metadata.imageModelName }}
          </span>
          <span v-if="node.metadata.imageSize" class="metric-chip">
            📐 {{ node.metadata.imageSize }}
          </span>
        </template>
        <!-- Mail send -->
        <template v-if="node.metadata?.type === 'mail'">
          <span v-if="node.metadata.sendSuccess === true" class="bg-green-50 text-green-600 px-1.5 py-0.5 rounded">
            ✉️ sent to {{ node.metadata.recipientCount }}
          </span>
        </template>
        <!-- Document extractor -->
        <template v-if="node.metadata?.type === 'document'">
          <span v-if="node.metadata.fileCount != null" class="metric-chip">
            📄 {{ node.metadata.fileCount }} files
          </span>
          <span v-if="node.metadata.extractedCharCount != null" class="metric-chip">
            📝 {{ node.metadata.extractedCharCount }} chars
          </span>
        </template>
      </div>
      <div class="flex flex-col space-y-2">
        <div class="text-base border-b border-gray-200 py-1">
          {{ t('common.input') }}
        </div>
        <div v-for="(content, name) in node.input" :key="`input_${name}`" class="flex">
          <div class="min-w-24 pr-2">
            {{ name }}
          </div>
          <div>
            {{ content.value || t('common.noContent') }}
          </div>
        </div>
        <div class="text-base border-b border-gray-200 py-1">
          {{ t('common.output') }}
        </div>
        <div v-for="(content, name) in node.output" :key="`onput_${name}`" class="flex">
          <template v-if="content.type === 4">
            <NImageGroup>
              <NImage
                v-for="url in content.value" :key="url" :src="`${getRealFileUrl(url)}?token=${authStore.token}`"
                width="100"
              />
            </NImageGroup>
          </template>
          <template v-else>
            <div class="min-w-24 pr-2">
              {{ name }}
            </div>
            <div>
              <TextComponent :inversion="false" :text="content.value || t('common.noContent')" :as-raw-text="false" />
            </div>
          </template>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* Compact info chip used to display per-node observability metrics (tokens, model, counts...). */
.metric-chip {
  background-color: rgb(243 244 246); /* tailwind bg-gray-100 */
  padding: 0.125rem 0.375rem;          /* tailwind py-0.5 px-1.5 */
  border-radius: 0.25rem;              /* tailwind rounded */
}
</style>
