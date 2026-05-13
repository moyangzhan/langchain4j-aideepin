import { http } from '@/utils/http/axios'

function search(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/model/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function disable(id: string) {
  return http.request({
    url: '/admin/model/edit',
    method: 'post',
    data: {
      id,
      isEnable: false,
    },
  })
}

function enable(id: string) {
  return http.request({
    url: '/admin/model/edit',
    method: 'post',
    data: {
      id,
      isEnable: true,
    },
  })
}

function addOne(data) {
  return http.request({
    url: '/admin/model/addOne',
    method: 'post',
    data,
  })
}

function edit(data) {
  return http.request({
    url: '/admin/model/edit',
    method: 'post',
    data,
  })
}

function deleteOne(id: string) {
  return http.request({
    url: `/admin/model/del/${id}`,
    method: 'POST',
  })
}

export default {
  addOne,
  edit,
  search,
  disable,
  enable,
  deleteOne,
}
