import axios, { type AxiosResponse } from 'axios'
import { useAuthStore, useUserStore } from '@/store'

const service = axios.create({
  baseURL: import.meta.env.VITE_GLOB_API_URL,
})

service.interceptors.request.use(
  (config) => {
    const token = useAuthStore().token
    if (token)
      config.headers.Authorization = `${token}`

    return config
  },
  (error) => {
    console.error(`request error:${error}`)
    return Promise.reject(error.response)
  },
)

service.interceptors.response.use(
  (response: AxiosResponse): AxiosResponse => {
    if (response.status === 200)
      return response

    throw new Error(response.status.toString())
  },
  (error) => {
    if (error.response.status === 401) {
      console.log('无登录权限')
      const authStore = useAuthStore()
      authStore.removeToken()
      const userStore = useUserStore()
      userStore.resetUserInfo()
    }
    console.error(`interceptors error:${error}`)
    return Promise.reject(error)
  },
)

export default service
