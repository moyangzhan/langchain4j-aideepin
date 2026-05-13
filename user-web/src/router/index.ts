import type { App } from 'vue'
import type { RouteRecordRaw } from 'vue-router'
import { createRouter, createWebHashHistory } from 'vue-router'
import { setupPageGuard } from './permission'
import { ChatLayout } from '@/views/chat/layout'
import { KnowledgeBaseLayout } from '@/views/knowledge-base/layout'
import { WorkflowBaseLayout } from '@/views/workflow/layout'

const routes: RouteRecordRaw[] = [
  {
    path: '/',
    name: 'Root',
    redirect: '/chat/default',
  },
  {
    path: '/chat',
    name: 'Chat',
    component: ChatLayout,
    children: [
      {
        path: ':uuid',
        name: 'ChatDetail',
        component: () => import('@/views/chat/index.vue'),
      },
    ],
  },
  {
    path: '/active',
    name: 'Active',
    component: () => import('@/views/user/Active.vue'),
  },
  {
    path: '/draw',
    name: 'Draw',
    component: () => import('@/views/draw/index.vue'),
  },
  {
    path: '/gallery',
    name: 'Gallery',
    component: () => import('@/views/gallery/index.vue'),
  },
  {
    path: '/qa',
    component: KnowledgeBaseLayout,
    name: 'QAIndex',
    children: [
      {
        path: ':kbUuid',
        name: 'QADetail',
        component: () => import('@/views/knowledge-base/index.vue'),
      },
    ],
  },
  {
    path: '/ai-search',
    name: 'AiSearch',
    component: () => import('@/views/ai-search/index.vue'),
  },
  {
    path: '/kb-manage',
    name: 'KnowledgeBaseManage',
    component: () => import('@/views/knowledge-base-manage/index.vue'),
  },
  {
    path: '/kb-manage/:kbUuid',
    name: 'KnowledgeBaseManageDetail',
    component: () => import('@/views/knowledge-base-manage/KnowledgeBaseDetail.vue'),
  },
  {
    path: '/workflow',
    component: WorkflowBaseLayout,
    name: 'WfIndex',
    children: [
      {
        path: ':uuid',
        name: 'WfDetail',
        component: () => import('@/views/workflow/index.vue'),
      },
    ],
  },
  {
    path: '/mcp',
    name: 'Mcp',
    component: () => import('@/views/mcp/index.vue'),
  },
  {
    path: '/404',
    name: '404',
    component: () => import('@/views/exception/404/index.vue'),
  },

  {
    path: '/500',
    name: '500',
    component: () => import('@/views/exception/500/index.vue'),
  },

  {
    path: '/:pathMatch(.*)*',
    name: 'notFound',
    redirect: '/404',
  },
]

export const router = createRouter({
  history: createWebHashHistory(),
  routes,
  scrollBehavior: () => ({ left: 0, top: 0 }),
})

setupPageGuard(router)

export async function setupRouter(app: App) {
  app.use(router)
  await router.isReady()
}
