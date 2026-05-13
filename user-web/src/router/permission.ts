import type { Router } from 'vue-router'
import { useAuthStoreWithout } from '@/store/modules/auth'

export function setupPageGuard(router: Router) {
  router.beforeEach(async (to, from, next) => {
    const authStore = useAuthStoreWithout()
    if (!authStore.token) {
      console.info('not token')
      try {
        if (to.path === '/500')
          next({ name: 'Root' })
        else
          next()
      } catch (error) {
        console.error(error)
        if (to.path !== '/500')
          next({ name: '500' })
        else
          next()
      }
    } else {
      next()
    }
  })
}
