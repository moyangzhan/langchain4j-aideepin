<script setup lang='ts'>
import { computed, ref } from 'vue'
import { NButton, NCollapse, NCollapseItem, NDivider, NFlex, NH4, NSpace, NTable, useMessage } from 'naive-ui'
import { t } from '@/locales'
import { useCharacterDoc } from '@/hooks/api-doc/useCharacterDoc'
import { useKnowledgeDoc } from '@/hooks/api-doc/useKnowledgeDoc'
import { useWorkflowDoc } from '@/hooks/api-doc/useWorkflowDoc'

interface Props {
  type: string
  uuid?: string
  wfInputDefs?: Workflow.NodeIODefinition[]
}
const props = defineProps<Props>()
const ms = useMessage()
const copied = ref(false)

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
  return useWorkflowDoc(urlBase, props.wfInputDefs)
})

function handleCopyCurl() {
  navigator.clipboard.writeText(endpointInfo.value.curlExample).then(() => {
    copied.value = true
    setTimeout(() => copied.value = false, 2000)
  }).catch(() => ms.error(t('common.wrong')))
}
</script>

<template>
  <NSpace vertical :size="12">
    <NDivider style="margin: 4px 0" />
    <NH4 style="margin: 0">
      {{ t('extApi.docTitle') }}
    </NH4>

    <NCollapse>
      <NCollapseItem :title="t('extApi.docEndpoint')" name="endpoint">
        <NTable :bordered="true" :single-line="false" size="small">
          <tbody>
            <tr>
              <td style="width: 100px; font-weight: 600">
                {{ t('extApi.docMethod') }}
              </td>
              <td>{{ endpointInfo.method }}</td>
            </tr>
            <tr>
              <td style="font-weight: 600">
                URL
              </td>
              <td style="word-break: break-all">
                {{ endpointInfo.endpoint }}
              </td>
            </tr>
            <tr>
              <td style="font-weight: 600">
                Auth
              </td>
              <td>Authorization: YOUR_API_KEY</td>
            </tr>
          </tbody>
        </NTable>
      </NCollapseItem>

      <NCollapseItem :title="t('extApi.docParams')" name="params">
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
            <tr v-for="p in endpointInfo.params" :key="p.name">
              <td>{{ p.name }}</td>
              <td>{{ p.type }}</td>
              <td>{{ p.required }}</td>
              <td v-html="p.desc" />
            </tr>
          </tbody>
        </NTable>
      </NCollapseItem>

      <NCollapseItem :title="t('extApi.docResponse')" name="response">
        <NTable v-if="endpointInfo.responseFields.length > 0" :bordered="true" :single-line="false" size="small" style="margin-bottom: 8px">
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
            <tr v-for="f in endpointInfo.responseFields" :key="f.name">
              <td>{{ f.name }}</td>
              <td>{{ f.type }}</td>
              <td v-html="f.desc" />
            </tr>
          </tbody>
        </NTable>
        <pre class="text-xs whitespace-pre-wrap break-all bg-gray-100 dark:bg-gray-800 rounded p-3">{{ endpointInfo.responseExample }}</pre>
      </NCollapseItem>

      <NCollapseItem name="curl">
        <template #header>
          <NFlex align="center" justify="space-between" :size="8" style="width:100%">
            <span>cURL</span>
            <NButton size="tiny" @click.stop="handleCopyCurl">
              {{ copied ? t('chat.copied') : t('chat.copy') }}
            </NButton>
          </NFlex>
        </template>
        <pre class="text-xs whitespace-pre-wrap break-all bg-gray-100 dark:bg-gray-800 rounded p-3">{{ endpointInfo.curlExample }}</pre>
      </NCollapseItem>
    </NCollapse>
  </NSpace>
</template>
