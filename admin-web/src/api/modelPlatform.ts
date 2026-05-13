import { http } from '@/utils/http/axios'

function search(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/model-platform/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function addOne(data) {
  return http.request({
    url: '/admin/model-platform/add',
    method: 'post',
    data,
  })
}

function edit(data) {
  return http.request({
    url: '/admin/model-platform/edit',
    method: 'post',
    data,
  })
}

function deleteOne(id: string) {
  return http.request({
    url: `/admin/model-platform/del/${id}`,
    method: 'POST',
  })
}

export default {
  addOne,
  edit,
  search,
  deleteOne,
}
