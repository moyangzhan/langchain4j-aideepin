import { http } from '@/utils/http/axios'
import { McpSearchReq, McpAddOrEditReq } from '/#/mcp'

function mcpSearch(data: McpSearchReq, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/mcp/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function mcpAdd(data: McpAddOrEditReq) {
  return http.request({
    url: '/admin/mcp/add',
    method: 'post',
    data,
  })
}

function mcpEdit(data: McpAddOrEditReq) {
  return http.request({
    url: '/admin/mcp/edit',
    method: 'post',
    data,
  })
}

function mcpDel(uuid: string) {
  return http.request({
    url: `/admin/mcp/del/${uuid}`,
    method: 'post',
  })
}

function mcpSetEnable(params: { uuid: string; isEnable: boolean }) {
  return http.request({
    url: `/admin/mcp/enable?uuid=${params.uuid}&isEnable=${params.isEnable}`,
    method: 'post',
  })
}

export default {
  mcpSearch,
  mcpAdd,
  mcpEdit,
  mcpDel,
  mcpSetEnable,
}
