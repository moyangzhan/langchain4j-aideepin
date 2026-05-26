<script lang="ts" setup>
import { NImage, NImageGroup } from 'naive-ui'
import { SvgIcon } from '@/components/common'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { useAuthStore, useWfStore } from '@/store'
import { t } from '@/locales'
import { getRealFileUrl } from '@/utils/functions'
import TextComponent from '@/views/chat/components/Message/Text.vue'

interface Props {
  nodes: Workflow.WfRuntimeNode[]
  workflow: Workflow.WorkflowInfo
  errorMsg: string
}
const props = defineProps<Props>()
const wfStore = useWfStore()
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
