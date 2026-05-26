export interface ApiParamDef {
  name: string
  type: string
  required: string
  desc: string
}

export interface ApiEndpointInfo {
  endpoint: string
  method: string
  description: string
  params: ApiParamDef[]
  responseFields: ApiParamDef[]
  responseExample: string
  curlExample: string
}
