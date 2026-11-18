import { http } from '@/utils/http/axios'

function search(kbUuid: string, keyword: string, params: { current: number; size: number }) {
  const kb = kbUuid ? `&kbUuid=${encodeURIComponent(kbUuid)}` : ''
  const kw = keyword ? `&keyword=${encodeURIComponent(keyword)}` : ''
  return http.request({
    url: `/admin/document/search?currentPage=${params.current}&pageSize=${params.size}${kb}${kw}`,
    method: 'get',
  })
}

function deleteOne(uuid: string) {
  return http.request({
    url: `/admin/document/del/${uuid}`,
    method: 'POST',
  })
}

function edit(data) {
  return http.request({
    url: '/admin/document/edit',
    method: 'post',
    data,
  })
}

export default {
  search,
  edit,
  deleteOne,
}
