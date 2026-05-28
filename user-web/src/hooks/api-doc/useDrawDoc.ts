import type { ApiEndpointInfo } from './types'
import { t } from '@/locales'

export function useDrawDoc(urlBase: string): ApiEndpointInfo {
  return {
    endpoints: [
      {
        title: t('extApi.docDrawTitle'),
        description: t('extApi.docDrawDesc'),
        endpoint: `${urlBase}/draw/generation`,
        method: 'POST',
        params: [
          { name: 'prompt', type: 'string', required: t('common.yes'), desc: t('extApi.docParamPrompt') },
          { name: 'negative_prompt', type: 'string', required: t('common.no'), desc: t('extApi.docParamNegativePrompt') },
          { name: 'size', type: 'string', required: t('common.no'), desc: t('extApi.docParamSize') },
          { name: 'quality', type: 'string', required: t('common.no'), desc: t('extApi.docParamQuality') },
          { name: 'number', type: 'integer', required: t('common.no'), desc: t('extApi.docParamNumber') },
          { name: 'model', type: 'string', required: t('common.no'), desc: t('extApi.docParamModel') },
          { name: 'seed', type: 'integer', required: t('common.no'), desc: t('extApi.docParamSeed') },
        ],
        responseFields: [
          { name: 'uuid', type: 'string', required: '', desc: t('extApi.docDrawUuid') },
        ],
        responseExample: `{
  "success": true,
  "data": {
    "uuid": "abc123def456"
  }
}`,
        curlExample: `curl -X POST '${urlBase}/draw/generation' \\
  -H 'Authorization: YOUR_API_KEY' \\
  -H 'Content-Type: application/json' \\
  -d '{
    "prompt": "a cute cat playing piano",
    "model": "gpt-image-2",
    "number": 1
  }'`,
      },
    ],
  }
}
