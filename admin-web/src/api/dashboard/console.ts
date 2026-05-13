import { http } from '@/utils/http/axios'

//获取主控台信息
export function getStatistic() {
  return http.request({
    url: '/admin/statistic/info',
    method: 'get',
  })
}
