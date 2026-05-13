import { defineStore } from 'pinia'
import { getToken, removeToken, setToken } from './helper'
import { store } from '@/store'

export const useAuthStore = defineStore('auth-store', {
  state: (): AuthState => ({
    token: getToken(),
    showLoginModal: false,
  }),

  getters: {
  },

  actions: {

    setToken(token: string) {
      document.cookie = `Authorization=${token}`
      this.token = token
      this.showLoginModal = false
      setToken(token)
    },

    removeToken() {
      console.log('remove token')
      this.token = ''
      removeToken()
    },

    setLoginView(show: boolean) {
      console.log('set login view', show)
      this.showLoginModal = show
    },

    checkLoginOrShow() {
      if (!this.token) {
        this.showLoginModal = true
        return false
      }
      return true
    },
  },
})

export function useAuthStoreWithout() {
  return useAuthStore(store)
}
