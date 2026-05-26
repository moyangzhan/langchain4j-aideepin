import type { ApiEndpointInfo } from './types'
import { t } from '@/locales'

const TYPE_MAP: Record<number, string> = { 1: 'Text', 2: 'Number', 3: 'Options', 4: 'Files', 5: 'Bool' }

export function useWorkflowDoc(urlBase: string, inputDefs?: Workflow.NodeIODefinition[]): ApiEndpointInfo {
  const hasInputs = inputDefs && inputDefs.length > 0

  const inputsDesc = hasInputs
    ? `<pre class="text-xs m-0" style="white-space:pre-wrap">${inputDefs!.map(d => `{
  "name": "${d.name}",
  "content": {
    "type": ${d.type},  // ${TYPE_MAP[d.type] ?? ''}
    "value": ""
  }${d.required ? `  // ${t('extApi.docRequired')}` : ''}
}`).join('\n')}</pre>`
    : t('extApi.docParamInputs')

  const inputsExample = hasInputs
    ? inputDefs!.map((d, i) => `      {
        "name": "${d.name}",
        "content": {
          "type": ${d.type},
          "value": ""
        }
      }${i < inputDefs!.length - 1 ? ',' : ''}`).join('\n')
    : `      {
        "name": "text",
        "content": {
          "type": 1,
          "value": "Analyze this content"
        }
      }`

  return {
    endpoint: `${urlBase}/workflow/run`,
    method: 'POST',
    description: t('extApi.docWfDesc'),
    params: [
      { name: 'inputs', type: 'array', required: t('common.no'), desc: inputsDesc },
      { name: 'response_mode', type: 'string', required: t('common.no'), desc: t('extApi.docParamMode') },
    ],
    responseFields: [
      { name: 'task_id', type: 'string', required: '', desc: t('extApi.docRespTaskId') },
      { name: 'status', type: 'string', required: '', desc: t('extApi.docRespStatus') },
      { name: 'outputs', type: 'object', required: '', desc: t('extApi.docParamOutputs') },
    ],
    responseExample: `{
  "success": true,
  "data": {
    "task_id": "f8e7d6c5b4a3",
    "status": "completed",
    "outputs": {
      "<param_name>": {
        "type": 1,
        "title": "<param_title>",
        "value": "Analysis completed successfully"
      }
    }
  }
}`,
    curlExample: `curl -X POST '${urlBase}/workflow/run' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "inputs": [
${inputsExample}
    ],
    "response_mode": "blocking"
  }'`,
  }
}
