import { RouteRecordRaw } from 'vue-router'
import { Layout } from '@/router/constant'
import { CodeSandboxOutlined } from '@vicons/antd'
import { renderIconWithProps } from '@/utils/index'

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
    path: '/ai-model',
    name: 'AiModel',
    redirect: '/ai-model/list',
    component: Layout,
    meta: {
      title: 'route.modelManagement',
      icon: renderIconWithProps(CodeSandboxOutlined, { size: 24 }),
      sort: 3,
    },
    children: [
      {
        path: 'platform',
        name: 'AiModelPlatform',
        meta: {
          title: 'route.platformConfig',
          activeMenu: 'AiModelPlatform',
        },
        component: () => import('@/views/ai-model/platform.vue'),
      },
      {
        path: 'list',
        name: 'AiModelList',
        meta: {
          title: 'route.modelList',
        },
        component: () => import('@/views/ai-model/index.vue'),
      },
    ],
  },
]

export default routes
