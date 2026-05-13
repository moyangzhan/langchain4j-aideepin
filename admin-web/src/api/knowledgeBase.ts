import { http } from '@/utils/http/axios'

function search(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/kb/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function deleteOne(uuid: string) {
  return http.request({
    url: `/admin/kb/del/${uuid}`,
    method: 'POST',
  })
}

function edit(data) {
  return http.request({
    url: '/admin/kb/edit',
    method: 'post',
    data,
  })
}

/**
 * @description: 获取用户信息
 */
function getInfo(uuid: string) {
  return http.request({
    url: `/admin/kb/info/${uuid}`,
    method: 'get',
  })
}

export default {
  search,
  getInfo,
  edit,
  deleteOne,
}
