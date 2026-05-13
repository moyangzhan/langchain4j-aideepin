<script setup lang='ts'>
import type { CSSProperties } from 'vue'
import { computed, onMounted, watch } from 'vue'
import { NButton, NLayoutSider } from 'naive-ui'
import List from './List.vue'
import { useAppStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { t } from '@/locales'

const appStore = useAppStore()

const { isMobile } = useBasicLayout()

const collapsed = computed(() => appStore.siderCollapsed)

function handleUpdateCollapsed() {
  appStore.setSiderCollapsed(!collapsed.value)
}

const getMobileClass = computed<CSSProperties>(() => {
  if (isMobile.value) {
    return {
      position: 'fixed',
      zIndex: 50,
    }
  }
  return {}
})

const mobileSafeArea = computed(() => {
  if (isMobile.value) {
    return {
      paddingBottom: 'env(safe-area-inset-bottom)',
    }
  }
  return {}
})

watch(
  isMobile,
  (val) => {
    appStore.setSiderCollapsed(val)
  },
  {
    immediate: true,
    flush: 'post',
  },
)

onMounted(async () => {
  console.info('kb index,onmounted')
})
</script>

<template>
  <NLayoutSider
    :collapsed="collapsed" :collapsed-width="0" :width="260" :show-trigger="isMobile ? false : true"
    position="absolute" bordered :style="getMobileClass" @update-collapsed="handleUpdateCollapsed"
  >
    <div class="relative flex flex-col h-full" :style="mobileSafeArea">
      <main class="flex flex-col flex-1 min-h-0 pb-16">
        <List class="flex-1 min-h-0" />
      </main>
      <div class="absolute bottom-0 left-0 right-0 px-4 py-3 border-t border-gray-200 bg-white dark:border-neutral-800 dark:bg-[#18181c]">
        <NButton secondary block @click="$router.push({ name: 'KnowledgeBaseManage' })">
          {{ t('chat.knowledgeBaseManage') }}
        </NButton>
      </div>
    </div>
  </NLayoutSider>
  <template v-if="isMobile">
    <div v-show="!collapsed" class="fixed inset-0 z-40 bg-black/40" @click="handleUpdateCollapsed" />
  </template>
</template>
