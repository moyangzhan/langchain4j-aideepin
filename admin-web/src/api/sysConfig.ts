import { http } from '@/utils/http/axios'

function search(
  data: { keyword?: string; names?: string[] },
  currentPage: number,
  pageSize: number
) {
  return http.request({
    url: `/admin/sys-config/search?currentPage=${currentPage}&pageSize=${pageSize}`,
    method: 'post',
    data,
  })
}

function edit(params) {
  return http.request({
    url: '/admin/sys-config/edit',
    method: 'post',
    params,
  })
}

function deleteOne(id: string) {
  return http.request({
    url: `/admin/model/del/${id}`,
    method: 'POST',
  })
}

export default {
  search,
  edit,
  deleteOne,
}
