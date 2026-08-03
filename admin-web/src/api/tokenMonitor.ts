import { http } from '@/utils/http/axios'

function search(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/token-monitor/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

export default {
  search,
}
