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

const props = withDefaults(defineProps<Props>(), {
  draws: () => [],
  delBtnEnable: true,
})
const emit = defineEmits<Emit>()
const modalMainHeight = ref<number>(600)
const { scrollRef, scrollToTop, scrollToBottom } = useScroll()
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
}

function openDraw(item: Chat.Draw) {
  selectedDraw.value = item
  emit('clickDraw', item)
}

async function handleScroll(event: any) {
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
</template>

<style lang="less" scoped>
:deep(.n-image) {
  width: 100%;
}

:deep(.n-image img) {
  width: 100%;
}
</style>
