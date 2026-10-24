<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { NEmpty, NIcon, NImage, NSpin } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { Reload } from '@vicons/ionicons5'
import { useScroll } from '@/views/chat/hooks/useScroll'
import { useAuthStore, useDrawStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import LoginTip from '@/views/user/LoginTip.vue'
import NoPic from '@/assets/no_pic.png'
import { emptyDraw, getRealFileUrl } from '@/utils/functions'
import { t } from '@/locales'
import { SvgIcon } from '@/components/common'
const props = withDefaults(defineProps<Props>(), {
  draws: () => [],
  delBtnEnable: true,
  scrollShortcut: 'none',
})
const emit = defineEmits<Emit>()
const modalMainHeight = ref<number>(600)
const { scrollRef, isAtBottom, scrollToTop, scrollToBottom, checkAtBottom } = useScroll()
// 顶部快捷按钮的显隐判断(与 isAtBottom 对称,本地维护)
// Visibility for the top shortcut, symmetric to isAtBottom, tracked locally
const isAtTop = ref(true)
const authStore = useAuthStore()
const { isMobile } = useBasicLayout()
const drawStore = useDrawStore()
const selectedDraw = ref<Chat.Draw>(emptyDraw())
let prevScrollTop = 0

interface Emit {
  (e: 'loadMore', callback: Function): void
  (e: 'clickDraw', draw: Chat.Draw): void
}

interface Props {
  draws: Chat.Draw[]
  showDelBtn?: boolean
  loginBtnEnable?: boolean
  /** 滚动快捷按钮方向:none 不展示;top 回到顶部(新图在顶部的列表);bottom 回到底部(新图在底部的列表) / Scroll shortcut direction: none; top (newest-at-top lists); bottom (newest-at-bottom lists) */
  scrollShortcut?: 'none' | 'top' | 'bottom'
}

function openDraw(item: Chat.Draw) {
  selectedDraw.value = item
  emit('clickDraw', item)
}

async function handleScroll(event: any) {
  checkAtBottom()
  isAtTop.value = event.target.scrollTop < 50
  const scrollTop = event.target.scrollTop
  if (prevScrollTop < scrollTop && event.target.scrollHeight - event.target.scrollTop - event.target.clientHeight < 50) {
    emit('loadMore', () => {
      console.log('gallery loaded')
    })
  }
  prevScrollTop = scrollTop
}

onMounted(() => {
  modalMainHeight.value = (window.innerHeight - 110)
})

function gotoTop() {
  scrollToTop()
}

function gotoBottom() {
  scrollToBottom()
}
defineExpose({ gotoTop, gotoBottom })
</script>

<template>
  <div class="relative h-full">
    <div ref="scrollRef" class="h-full overflow-y-auto" @scroll="handleScroll">
    <div class="w-full max-w-screen-xl m-auto dark:bg-[#101014]" :class="[isMobile ? 'p-2' : 'p-4']">
      <template v-if="props.draws.length === 0">
        <div v-if="!loginBtnEnable" class="flex items-center justify-center mt-4 text-center text-neutral-400">
          <NIcon :component="Cat" size="32" />
          <span class="pl-1">Roar~</span>
        </div>
        <LoginTip v-if="loginBtnEnable && !authStore.token" />
      </template>
      <template v-else>
        <masonry :cols="{ default: 5, 1000: 4, 700: 3, 400: 1 }" :gutter="{ default: '15px', 700: '10px' }">
          <template v-for="draw of props.draws" :key="draw.uuid">
            <template v-if="draw.uuid === drawStore.loadingUuid">
              <NSpin size="medium">
                <template #icon>
                  <NIcon>
                    <Reload />
                  </NIcon>
                </template>
              </NSpin>
            </template>
            <template v-else>
              <template v-if="draw.uuid !== drawStore.loadingUuid && draw.processStatus === 2">
                <NEmpty :description="t('draw.drawFailed')" />
              </template>
              <template
                v-else-if="draw.uuid !== drawStore.loadingUuid && (!draw.imageUrls || draw.imageUrls.length === 0)"
              >
                <NEmpty :description="t('draw.imageNotFound')" />
              </template>
              <template v-else-if="draw.imageUrls && draw.imageUrls.length > 0">
                <template v-for="imageUrl in draw.imageUrls" :key="imageUrl">
                  <NImage
                    v-if="imageUrl && draw.uuid !== drawStore.loadingUuid" width="100%"
                    :src="`${getRealFileUrl(imageUrl)}?token=${authStore.token}`" :fallback-src="NoPic" preview-disabled
                    @click="openDraw(draw)"
                  />
                </template>
              </template>
            </template>
          </template>
        </masonry>
      </template>
    </div>
  </div>
  <!-- 滚动快捷按钮:top=回到顶部(新图在顶部的列表),bottom=回到底部(新图在底部的列表) / Scroll shortcut: top for newest-at-top lists, bottom for newest-at-bottom lists -->
    <Transition name="scroll-to-bottom">
      <button
        v-if="props.scrollShortcut === 'top' && !isAtTop && props.draws.length > 0"
        type="button"
        class="absolute bottom-4 right-4 z-10 flex items-center justify-center w-8 h-8 rounded-full border border-neutral-200 bg-white/90 shadow-md transition-colors hover:bg-white dark:border-neutral-700 dark:bg-neutral-800/90 dark:hover:bg-neutral-800"
        :aria-label="t('draw.scrollToTop')"
        @click="scrollToTop()"
      >
        <SvgIcon icon="ri:arrow-up-s-line" class="text-lg" />
      </button>
      <button
        v-else-if="props.scrollShortcut === 'bottom' && !isAtBottom && props.draws.length > 0"
        type="button"
        class="absolute bottom-4 right-4 z-10 flex items-center justify-center w-8 h-8 rounded-full border border-neutral-200 bg-white/90 shadow-md transition-colors hover:bg-white dark:border-neutral-700 dark:bg-neutral-800/90 dark:hover:bg-neutral-800"
        :aria-label="t('chat.scrollToBottom')"
        @click="scrollToBottom(true)"
      >
        <SvgIcon icon="ri:arrow-down-s-line" class="text-lg" />
      </button>
    </Transition>
  </div>
</template>

<style lang="less" scoped>
.scroll-to-bottom-enter-active,
.scroll-to-bottom-leave-active {
  transition: all 0.2s ease;
}
.scroll-to-bottom-enter-from,
.scroll-to-bottom-leave-to {
  opacity: 0;
  transform: translateY(8px);
}
:deep(.n-image) {
  width: 100%;
}

:deep(.n-image img) {
  width: 100%;
}
</style>
