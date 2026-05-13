import { RouteRecordRaw } from 'vue-router'
import { Layout } from '@/router/constant'
import { OptionsSharp } from '@vicons/ionicons5'
import { renderIcon } from '@/utils/index'

/**
 * @param name 路由名称, 必须设置,且不能重名
 * @param meta 路由元信息（路由附带扩展信息）
 * @param redirect 重定向地址, 访问这个路由时,自定进行重定向
 * @param meta.disabled 禁用整个菜单
 * @param meta.title 菜单名称
 * @param meta.icon 菜单图标
 * @param meta.keepAlive 缓存该路由
 * @param meta.sort 排序越小越排前
 *
 * */
const routes: Array<RouteRecordRaw> = [
  {
    path: '/system',
    name: 'System',
    redirect: '/system/quota',
    component: Layout,
    meta: {
      title: 'route.systemSettings',
      icon: renderIcon(OptionsSharp),
      sort: 6,
    },
    children: [
      {
        path: 'storage',
        name: 'storage',
        meta: {
          title: 'route.storageConfig',
        },
        component: () => import('@/views/system/storage/index.vue'),
      },
      {
        path: 'audio/asr',
        name: 'asr',
        meta: {
          title: 'route.asrConfig',
        },
        component: () => import('@/views/system/audio/Asr.vue'),
      },
      {
        path: 'audio/tts',
        name: 'tts',
        meta: {
          title: 'route.ttsConfig',
        },
        component: () => import('@/views/system/audio/Tts.vue'),
      },
      {
        path: 'quota',
        name: 'TokenQuota',
        meta: {
          title: 'route.quotaConfig',
        },
        component: () => import('@/views/system/quota/index.vue'),
      },
      {
        path: 'ratelimit',
        name: 'RateLimit',
        meta: {
          title: 'route.rateLimit',
        },
        component: () => import('@/views/system/ratelimit/index.vue'),
      },
    ],
  },
]

export default routes
