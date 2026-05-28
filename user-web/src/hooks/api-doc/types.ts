export interface ApiParamDef {
  name: string
  type: string
  required: string
  desc: string
}

export interface ApiEndpoint {
  title: string
  description: string
  endpoint: string
  method: string
  params: ApiParamDef[]
  responseFields: ApiParamDef[]
  responseExample: string
  curlExample: string
}

export interface ApiEndpointInfo {
  endpoints: ApiEndpoint[]
}
