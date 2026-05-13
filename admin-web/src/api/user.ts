import { http } from '@/utils/http/axios'
import { BasicResponseModel } from '/#/global'

/**
 * @description: 用户登录
 */
function login(params) {
  return http.request<BasicResponseModel>(
    {
      url: '/auth/login',
      method: 'POST',
      params,
    },
    {
      isShowMessage: false,
    }
  )
}

function search(data, params: { current: number; size: number }) {
  return http.request({
    url: `/admin/user/search?currentPage=${params.current}&pageSize=${params.size}`,
    method: 'post',
    data,
  })
}

/**
 * @description: 获取用户信息
 */
function getUserInfo(uuid: string) {
  return http.request({
    url: `/admin/user/info/${uuid}`,
    method: 'get',
  })
}

/**
 * @description: 用户修改密码
 */
function changePassword(params, uid) {
  return http.request(
    {
      url: `/admin/user/u${uid}/changepw`,
      method: 'POST',
      params,
    },
    {
      isTransformResponse: false,
    }
  )
}

function active(uuid: string) {
  return http.request({
    url: `/admin/user/active/${uuid}`,
    method: 'POST',
  })
}

function freeze(uuid: string) {
  return http.request({
    url: `/admin/user/freeze/${uuid}`,
    method: 'POST',
  })
}

function addOne(data) {
  return http.request({
    url: '/admin/user/addOne',
    method: 'post',
    data,
  })
}

function edit(data) {
  return http.request({
    url: '/admin/user/edit',
    method: 'post',
    data,
  })
}

/**
 * @description: 用户登出
 */
function logout() {
  return http.request({
    url: '/user/logout',
    method: 'POST',
  })
}

export default {
  login,
  search,
  active,
  freeze,
  addOne,
  edit,
  getUserInfo,
  changePassword,
  logout,
}
