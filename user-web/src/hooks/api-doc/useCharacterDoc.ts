import type { ApiEndpointInfo } from './types'
import { t } from '@/locales'

export function useCharacterDoc(urlBase: string): ApiEndpointInfo {
  return {
    endpoints: [
      {
        title: t('extApi.docChatTitle'),
        description: t('extApi.docChatDesc'),
        endpoint: `${urlBase}/character`,
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
    "message_id": "a1b2c3d4e5f6",
    "answer": "Hello! How can I help you today?",
    "usage": {
      "prompt_tokens": 10,
      "completion_tokens": 8,
      "total_tokens": 18
    }
  }
}`,
        curlExample: `curl -X POST '${urlBase}/character' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "query": "Hello",
    "model": "gpt-5.4-mini",
    "response_mode": "streaming"
  }'`,
      },
    ],
  }
}
