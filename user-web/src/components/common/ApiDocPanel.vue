<script setup lang='ts'>
import { computed, ref } from 'vue'
import { NButton, NCollapse, NCollapseItem, NFlex, NH4, NSpace, NTable, useMessage } from 'naive-ui'
import ApiDocCommon from './ApiDocCommon.vue'
import { t } from '@/locales'
import { useCharacterDoc } from '@/hooks/api-doc/useCharacterDoc'
import { useKnowledgeDoc } from '@/hooks/api-doc/useKnowledgeDoc'
import { useWorkflowDoc } from '@/hooks/api-doc/useWorkflowDoc'
import { useDrawDoc } from '@/hooks/api-doc/useDrawDoc'
import { useMcpDoc } from '@/hooks/api-doc/useMcpDoc'

interface Props {
  type: string
  uuid?: string
  wfInputDefs?: Workflow.NodeIODefinition[]
}
const props = defineProps<Props>()
const ms = useMessage()
const copiedIndex = ref(-1)

const baseUrl = computed(() => {
  const apiBase = import.meta.env.VITE_GLOB_API_URL || ''
  const base = apiBase.replace(/\/$/, '')
  return `${window.location.origin}${base}/ext/v1`
})

const endpointInfo = computed(() => {
  const urlBase = baseUrl.value
  if (props.type === 'character')
    return useCharacterDoc(urlBase)
  if (props.type === 'knowledge')
    return useKnowledgeDoc(urlBase)
  if (props.type === 'draw')
    return useDrawDoc(urlBase)
  if (props.type === 'mcp')
    return useMcpDoc(urlBase)
  return useWorkflowDoc(urlBase, props.wfInputDefs)
})

const hasMultipleEndpoints = computed(() => endpointInfo.value.endpoints.length > 1)

function handleCopyCurl(index: number, curlExample: string) {
  navigator.clipboard.writeText(curlExample).then(() => {
    copiedIndex.value = index
    setTimeout(() => copiedIndex.value = -1, 2000)
  }).catch(() => ms.error(t('common.wrong')))
}
</script>

<template>
  <NSpace vertical :size="12">
    <NDivider style="margin: 4px 0" />
    <NH4 style="margin: 0">
      {{ t('extApi.docTitle') }}
    </NH4>

    <ApiDocCommon />

    <NH4 v-if="hasMultipleEndpoints" style="margin: 0">
      {{ t('extApi.docEndpointList') }}
    </NH4>

    <div
      v-for="(endpoint, index) in endpointInfo.endpoints"
      :id="`endpoint-${index}`"
      :key="index"
      class="border dark:border-gray-700 rounded-lg p-4"
    >
      <NH4 style="margin: 0 0 4px 0">
        <span class="font-medium">{{ endpoint.title }}</span>
      </NH4>
      <p v-if="endpoint.description" class="text-sm text-gray-500 mt-0 mb-1">
        {{ endpoint.description }}
      </p>
      <NFlex align="center" :size="6" class="mb-3">
        <span class="px-2 py-0.5 bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300 rounded text-xs font-medium">
          {{ endpoint.method }}
        </span>
        <code class="text-xs text-gray-500">{{ endpoint.endpoint }}</code>
      </NFlex>

      <NCollapse>
        <NCollapseItem v-if="endpoint.params.length > 0" :title="t('extApi.docParams')" name="params">
          <NTable :bordered="true" :single-line="false" size="small">
            <thead>
              <tr>
                <th style="min-width: 120px">
                  {{ t('extApi.docParamName') }}
                </th>
                <th style="min-width: 60px">
                  Type
                </th>
                <th style="min-width: 70px">
                  Required
                </th>
                <th>{{ t('extApi.docParamDesc') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="p in endpoint.params" :key="p.name">
                <td>{{ p.name }}</td>
                <td>{{ p.type }}</td>
                <td>{{ p.required }}</td>
                <td v-html="p.desc" />
              </tr>
            </tbody>
          </NTable>
        </NCollapseItem>

        <NCollapseItem :title="t('extApi.docResponse')" name="response">
          <NTable v-if="endpoint.responseFields.length > 0" :bordered="true" :single-line="false" size="small" style="margin-bottom: 8px">
            <thead>
              <tr>
                <th style="min-width: 120px">
                  {{ t('extApi.docParamName') }}
                </th>
                <th style="min-width: 60px">
                  Type
                </th>
                <th>{{ t('extApi.docParamDesc') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="f in endpoint.responseFields" :key="f.name">
                <td>{{ f.name }}</td>
                <td>{{ f.type }}</td>
                <td v-html="f.desc" />
              </tr>
            </tbody>
          </NTable>
          <pre class="text-xs whitespace-pre-wrap break-all bg-gray-100 dark:bg-gray-800 rounded p-3">{{ endpoint.responseExample }}</pre>
        </NCollapseItem>

        <NCollapseItem name="curl">
          <template #header>
            <NFlex align="center" justify="space-between" :size="8" style="width:100%">
              <span>cURL</span>
              <NButton size="tiny" @click.stop="handleCopyCurl(index, endpoint.curlExample)">
                {{ copiedIndex === index ? t('chat.copied') : t('chat.copy') }}
              </NButton>
            </NFlex>
          </template>
          <pre class="text-xs whitespace-pre-wrap break-all bg-gray-100 dark:bg-gray-800 rounded p-3">{{ endpoint.curlExample }}</pre>
        </NCollapseItem>
      </NCollapse>
    </div>
  </NSpace>
</template>
