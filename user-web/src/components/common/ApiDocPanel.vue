<script setup lang='ts'>
import { computed } from 'vue'
import { NCollapse, NCollapseItem, NCode, NDivider, NH4, NTable, NSpace } from 'naive-ui'
import { t } from '@/locales'

interface Props {
  type: string
}
const props = defineProps<Props>()

const baseUrl = computed(() => {
  const apiBase = import.meta.env.VITE_GLOB_API_URL || ''
  const base = apiBase.replace(/\/$/, '')
  return `${window.location.origin}${base}/api/v1`
})

const endpointInfo = computed(() => {
  const urlBase = baseUrl.value
  if (props.type === 'character') {
    return {
      endpoint: `${urlBase}/character/chat-messages`,
      method: 'POST',
      description: t('openApi.docChatDesc'),
      params: [
        { name: 'query', type: 'string', required: t('common.yes'), desc: t('openApi.docParamQuery') },
        { name: 'user', type: 'string', required: t('common.no'), desc: t('openApi.docParamUser') },
        { name: 'response_mode', type: 'string', required: t('common.no'), desc: t('openApi.docParamMode') },
      ],
      curlExample: `curl -X POST '${urlBase}/character/chat-messages' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "query": "Hello",
    "user": "user-123",
    "response_mode": "streaming"
  }'`,
    }
  }
  else if (props.type === 'kb') {
    return {
      endpoint: `${urlBase}/knowledge/qa`,
      method: 'POST',
      description: t('openApi.docKbDesc'),
      params: [
        { name: 'query', type: 'string', required: t('common.yes'), desc: t('openApi.docParamQuery') },
        { name: 'user', type: 'string', required: t('common.no'), desc: t('openApi.docParamUser') },
        { name: 'response_mode', type: 'string', required: t('common.no'), desc: t('openApi.docParamMode') },
      ],
      curlExample: `curl -X POST '${urlBase}/knowledge/qa' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "query": "What is RAG?",
    "response_mode": "streaming"
  }'`,
    }
  }
  else {
    return {
      endpoint: `${urlBase}/workflow/run`,
      method: 'POST',
      description: t('openApi.docWfDesc'),
      params: [
        { name: 'inputs', type: 'object', required: t('common.no'), desc: t('openApi.docParamInputs') },
        { name: 'user', type: 'string', required: t('common.no'), desc: t('openApi.docParamUser') },
        { name: 'response_mode', type: 'string', required: t('common.no'), desc: t('openApi.docParamMode') },
      ],
      curlExample: `curl -X POST '${urlBase}/workflow/run' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "inputs": { "text": "Analyze this content" },
    "response_mode": "blocking"
  }'`,
    }
  }
})
</script>

<template>
  <NSpace vertical :size="12">
    <NDivider style="margin: 4px 0" />
    <NH4 style="margin: 0">
      {{ t('openApi.docTitle') }}
    </NH4>

    <NCollapse>
      <NCollapseItem :title="t('openApi.docEndpoint')" name="endpoint">
        <NTable :bordered="true" :single-line="false" size="small">
          <tbody>
            <tr>
              <td style="width: 100px; font-weight: 600">
                {{ t('openApi.docMethod') }}
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

      <NCollapseItem :title="t('openApi.docParams')" name="params">
        <NTable :bordered="true" :single-line="false" size="small">
          <thead>
            <tr>
              <th>{{ t('openApi.docParamName') }}</th>
              <th>Type</th>
              <th>Required</th>
              <th>{{ t('openApi.docParamDesc') }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="p in endpointInfo.params" :key="p.name">
              <td>{{ p.name }}</td>
              <td>{{ p.type }}</td>
              <td>{{ p.required }}</td>
              <td>{{ p.desc }}</td>
            </tr>
          </tbody>
        </NTable>
      </NCollapseItem>

      <NCollapseItem title="cURL" name="curl">
        <NCode :code="endpointInfo.curlExample" language="bash" />
      </NCollapseItem>
    </NCollapse>
  </NSpace>
</template>
