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
      {
        title: t('extApi.docDrawDetailTitle'),
        description: t('extApi.docDrawDetailDesc'),
        endpoint: `${urlBase}/draw/{uuid}`,
        method: 'GET',
        params: [
          { name: 'uuid', type: 'string', required: t('common.yes'), desc: t('extApi.docDrawDetailUuid') },
        ],
        responseFields: [
          { name: 'uuid', type: 'string', required: '', desc: t('extApi.docDrawUuid') },
          { name: 'processStatus', type: 'integer', required: '', desc: t('extApi.docDrawProcessStatus') },
          { name: 'processStatusRemark', type: 'string', required: '', desc: t('extApi.docDrawProcessStatusRemark') },
          { name: 'prompt', type: 'string', required: '', desc: t('extApi.docDrawPrompt') },
          { name: 'generatedImages', type: 'string', required: '', desc: t('extApi.docDrawGeneratedImages') },
        ],
        responseExample: `{
  "success": true,
  "data": {
    "uuid": "abc123def456",
    "processStatus": 3,
    "processStatusRemark": "",
    "prompt": "a cute cat playing piano",
    "generatedImages": "file-uuid-1,file-uuid-2"
  }
}`,
        curlExample: `curl -X GET '${urlBase}/draw/abc123def456' \\
  -H 'Authorization: YOUR_API_KEY'`,
      },
    ],
  }
}
