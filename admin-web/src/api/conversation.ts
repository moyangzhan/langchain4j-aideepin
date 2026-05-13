import { http } from '@/utils/http/axios'

function searchPresetConvs(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/conv-preset/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function addPresetConv(data) {
  return http.request({
    url: '/admin/conv-preset/addOne',
    method: 'post',
    data,
  })
}

function editPresetConv(uuid: string, data) {
  return http.request({
    url: `/admin/conv-preset/edit/${uuid}`,
    method: 'post',
    data,
  })
}

function deletePresetConv(uuid: string) {
  return http.request({
    url: `/admin/conv-preset/del/${uuid}`,
    method: 'POST',
  })
}

function searchConvs(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/conv/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

function editConv(uuid: string, data) {
  return http.request({
    url: `/admin/conv/edit/${uuid}`,
    method: 'post',
    data,
  })
}

function deleteConv(uuid: string) {
  return http.request({
    url: `/admin/conv/del/${uuid}`,
    method: 'POST',
  })
}

export default {
  searchPresetConvs,
  addPresetConv,
  editPresetConv,
  deletePresetConv,
  searchConvs,
  editConv,
  deleteConv,
}
