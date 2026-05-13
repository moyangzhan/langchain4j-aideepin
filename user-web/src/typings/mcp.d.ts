declare namespace Mcp {

  interface McpPresetParam {
    name: string
    title: string
    value: any
    require_encrypt: boolean
    enctrypted: boolean
  }

  interface CustomizedParamDefinition {
    //接口定义字段
    name: string
    title: string
    require_encrypt: boolean

    //前端增加字段
    value: any
    enctrypted: boolean
  }

  interface McpInfo {
    id: string
    uuid: string
    title: string
    //sse | stdio
    transportType: string
    sseUrl: string
    sseTimeout: number
    stdioCommand: string
    stdioArg: string
    presetParams: McpPresetParam[]
    customizedParamDefinitions: CustomizedParamDefinition[]
    installType: string
    website: string
    remark: string
    isEnable: boolean

    // 前端增加字段
    // 当前用户是否配置了该MCP
    configured: boolean
  }

  interface McpCustomizedParam {
    //与McpInfo.customizedParamDefinitions中的name一一对应
    name: string
    value: any
    enctrypted: boolean
  }

  interface UserMcp {
    id: string
    uuid: string
    userId: string
    mcpId: string
    mcpCustomizedParams: McpCustomizedParam[]
    isEnable: boolean
    mcpInfo: McpInfo
  }

  interface UserMcpUpdateReq {
    mcpId: string
    mcpCustomizedParams: McpCustomizedParam[]
    isEnable: boolean
  }

  interface McpInfoListResp {
    total: number,
    pages: number,
    records: McpInfo[]
  }

  interface UserMcpListResp {
    total: number,
    pages: number,
    records: UserMcp[]
  }

  interface McpState {
    loading: boolean
    userMcpLoading: boolean
    myUserMcpList: UserMcp[]
  }
}