import { defineStore } from 'pinia'
import { store } from '@/store'
import { ACCESS_TOKEN, CURRENT_USER, IS_SCREENLOCKED } from '@/store/mutation-types'

import userApi from '@/api/user'
import { storage } from '@/utils/Storage'
import { UserInfoType, IUserState } from '/#/user'

export const useUserStore = defineStore({
  id: 'app-user',
  state: (): IUserState => ({
    token: storage.get(ACCESS_TOKEN, ''),
    name: '',
    welcome: '',
    avatar: '',
    permissions: [],
    info: storage.get(CURRENT_USER, {}),
  }),
  getters: {
    getToken(): string {
      return this.token
    },
    getAvatar(): string {
      return this.avatar
    },
    getPermissions(): [any][] {
      return this.permissions
    },
    getUserInfo(): UserInfoType {
      return this.info
    },
  },
  actions: {
    setToken(token: string) {
      this.token = token
    },
    setAvatar(avatar: string) {
      this.avatar = avatar
    },
    setPermissions(permissions) {
      this.permissions = permissions
    },
    setUserInfo(info: UserInfoType) {
      this.info = info
    },
    // 登录
    async login(params: any) {
      this.clearToken()
      const response = await userApi.login(params)
      const { data, success, message, code } = response
      if (success) {
        const ex = 7 * 24 * 60 * 60
        storage.set(ACCESS_TOKEN, data.token, ex)
        storage.set(CURRENT_USER, data, ex)
        storage.set(IS_SCREENLOCKED, false)
        storage.setCookie('Authorization', data.token)
        // document.cookie = `Authorization=${data.token}`
        this.setToken(data.token)
        this.setUserInfo(data)
      } else {
        console.log(`login fail, erroCode:${code},errorMessage:${message}`)
      }
      return response
    },

    // 登出
    async logout() {
      try {
        await userApi.logout()
      } catch (e) {
        console.error(e)
      } finally {
        this.clearToken()
      }
    },
    clearToken() {
      this.setPermissions([])
      this.setToken('')
      this.setUserInfo({ name: '', email: '', uuid: '' })
      storage.clear()
      storage.clearCookie()
    },
  },
})

// Need to be used outside the setup
export function useUser() {
  return useUserStore(store)
}
