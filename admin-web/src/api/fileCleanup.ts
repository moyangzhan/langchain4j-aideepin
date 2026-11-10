import { http } from '@/utils/http/axios'

function search(
  data: { name?: string; userName?: string },
  params: { current: number; size: number }
) {
  return http.request({
    url: `/admin/file/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data: { name: data.name || '', userName: data.userName || '' },
  })
}

// 静默业务失败（success=false 走 200 响应），被拒名单由页面统一汇总展示
// Silence the axios-layer toast: business rejections arrive as HTTP 200 and are summarized by the page
function deleteOne(uuid: string) {
  return http.request(
    {
      url: `/admin/file/del/${uuid}`,
      method: 'post',
    },
    { isShowMessage: false }
  )
}

export default {
  search,
  deleteOne,
}
