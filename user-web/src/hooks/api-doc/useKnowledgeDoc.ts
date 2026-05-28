import type { ApiEndpointInfo } from './types'
import { t } from '@/locales'

export function useKnowledgeDoc(urlBase: string): ApiEndpointInfo {
  return {
    endpoints: [
      {
        title: t('extApi.docKbTitle'),
        description: t('extApi.docKbDesc'),
        endpoint: `${urlBase}/knowledge`,
        method: 'POST',
        params: [
          { name: 'query', type: 'string', required: t('common.yes'), desc: t('extApi.docParamQuery') },
          { name: 'model', type: 'string', required: t('common.no'), desc: t('extApi.docParamModel') },
          { name: 'response_mode', type: 'string', required: t('common.no'), desc: t('extApi.docParamMode') },
        ],
        responseFields: [],
        responseExample: `{
  "success": true,
  "data": {
    "message_id": "29bf13e7e966489aa68c5d5b44f55d21",
    "answer": "RAG stands for Retrieval-Augmented Generation.",
    "usage": {
      "prompt_tokens": 11,
      "completion_tokens": 230,
      "total_tokens": 241
    }
  }
}`,
        curlExample: `curl -X POST '${urlBase}/knowledge' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "query": "What is RAG?",
    "model": "gpt-5.4-mini",
    "response_mode": "streaming"
  }'`,
      },
    ],
  }
}
