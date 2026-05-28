import type { ApiEndpointInfo } from './types'
import { t } from '@/locales'

export function useMcpDoc(urlBase: string): ApiEndpointInfo {
  return {
    endpoints: [
      {
        title: t('extApi.docMcpSupportedTitle'),
        description: t('extApi.docMcpSupportedDesc'),
        endpoint: `${urlBase}/mcp/supported`,
        method: 'GET',
        params: [],
        responseFields: [
          { name: 'uuid', type: 'string', required: '', desc: t('extApi.docMcpUuid') },
          { name: 'title', type: 'string', required: '', desc: t('extApi.docMcpTitle') },
          { name: 'transportType', type: 'string', required: '', desc: t('extApi.docMcpTransportType') },
          { name: 'sseUrl', type: 'string', required: '', desc: t('extApi.docMcpSseUrl') },
          { name: 'remark', type: 'string', required: '', desc: t('extApi.docMcpRemark') },
        ],
        responseExample: `{
  "success": true,
  "data": [
    {
      "uuid": "abc123",
      "title": "Web Search",
      "transportType": "sse",
      "sseUrl": "https://example.com/mcp/sse",
      "remark": "Web search tool"
    }
  ]
}`,
        curlExample: `curl -X GET '${urlBase}/mcp/supported' \\
  -H 'Authorization: YOUR_API_KEY'`,
      },
      {
        title: t('extApi.docMcpActiveTitle'),
        description: t('extApi.docMcpActiveDesc'),
        endpoint: `${urlBase}/mcp/active`,
        method: 'GET',
        params: [],
        responseFields: [
          { name: 'uuid', type: 'string', required: '', desc: t('extApi.docMcpUuid') },
          { name: 'title', type: 'string', required: '', desc: t('extApi.docMcpTitle') },
          { name: 'transportType', type: 'string', required: '', desc: t('extApi.docMcpTransportType') },
          { name: 'isEnable', type: 'boolean', required: '', desc: t('extApi.docMcpIsEnable') },
        ],
        responseExample: `{
  "success": true,
  "data": [
    {
      "uuid": "abc123",
      "title": "Web Search",
      "transportType": "sse",
      "isEnable": true
    }
  ]
}`,
        curlExample: `curl -X GET '${urlBase}/mcp/active' \\
  -H 'Authorization: YOUR_API_KEY'`,
      },
    ],
  }
}
