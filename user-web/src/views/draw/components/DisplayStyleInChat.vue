<script setup lang='ts'>
import { nextTick, ref, watch } from 'vue'
import { storeToRefs } from 'pinia'
import { NIcon, NModal, useDialog, useMessage } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { useScroll } from '../../chat/hooks/useScroll'
import Message from './Message/index.vue'
import DrawDetail from './DrawDetail.vue'
import LoginTip from '@/views/user/LoginTip.vue'
import { useAuthStore, useDrawStore, useGalleryStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { calcImageUrls, emptyDraw } from '@/utils/functions'
import { t } from '@/locales'
import api from '@/api'

const emit = defineEmits<Emit>()
const { scrollRef, scrollToBottom, scrollTo } = useScroll()
const { isMobile } = useBasicLayout()
const galleryStore = useGalleryStore()
const drawStore = useDrawStore()
const authStore = useAuthStore()
const dialog = useDialog()
const ms = useMessage()
const { myDraws } = storeToRefs<any>(drawStore)
const showDetailModal = ref<boolean>(false)
const selectedDraw = ref<Chat.Draw>(emptyDraw())
let prevScrollTop: number

interface Emit {
  (e: 'delOneImage', uuid: string, fileUrl: string): void
  (e: 'loadMore', callback: Function): void
}
async function handleScroll(event: any) {
  // 聊天模式时拉到最上面时加载新数据，画廊模式拉到最下面加载新数据
  const scrollTop = event.target.scrollTop
  if (
    scrollTop < 50
    && (scrollTop < prevScrollTop || prevScrollTop === undefined)
  ) {
    const lastScrollClient = event.target.scrollHeight
    emit('loadMore', () => {
      nextTick(() => {
        scrollTo(event.target.scrollHeight - lastScrollClient)
        prevScrollTop = event.target.scrollTop
      })
    })
  }
  prevScrollTop = scrollTop
}

watch(
  () => myDraws.value,
  (newVal, oldVal) => {
    if ((!oldVal || oldVal.length === 0) && newVal.length > 0) {
      console.log('displayStyleInChat scrollToBottom')
      scrollToBottom()
    }
  },
)
function openDraw(item: Chat.Draw) {
  showDetailModal.value = true
  selectedDraw.value = item
}
function handleDeleted() {
  showDetailModal.value = false
}
function handleDelDraw(item: Chat.Draw) {
  dialog.warning({
    title: `${t('draw.deleteDrawConfirm')}${item.prompt.substring(0, 11)}]?`,
    content: t('draw.deleteDrawContent'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: async () => {
      showDetailModal.value = false
      await api.drawDel<boolean>(item.uuid)
      drawStore.deleteDraw(item.uuid)
    },
  })
}
async function handleSetPublic(uuid: string, isPublic: boolean) {
  const ret = await api.drawSetPublic<Chat.Draw>(uuid, isPublic)
  if (ret) {
    calcImageUrls(ret.data)
    drawStore.setPublic(uuid, isPublic)
    galleryStore.setPublic(ret.data)
    ms.warning(t('draw.publicAccessChanged', { status: isPublic ? t('draw.publicAccessEnabled') : t('draw.publicAccessDisabled') }))
  }
}
function handleDelOneImage(uuid: string, fileUrl: string) {
  emit('delOneImage', uuid, fileUrl)
}

function gotoBottom() {
  scrollToBottom()
}

defineExpose({ gotoBottom })
</script>

<template>
  <div ref="scrollRef" class="h-full overflow-y-auto" @scroll="handleScroll">
    <div class="w-full max-w-screen-xl m-auto dark:bg-[#101014]" :class="[isMobile ? 'p-2' : 'p-4']">
      <LoginTip v-if="!authStore.token" />
      <template v-else-if="myDraws.length === 0">
        <div class="flex items-center justify-center mt-4 text-center text-neutral-400">
          <NIcon :component="Cat" size="32" />
          <span class="pl-1">Roar~</span>
        </div>
      </template>
      <template v-else>
        <div>
          <template v-for="draw of myDraws" :key="draw.uuid">
            <!-- 增加显示绘图统计数据，如starCount,isPublic -->
            <Message
              v-if="draw.interactingMethod === 1" :draw="draw" :inversion="true" type="text"
              @set-public="handleSetPublic" @delete="handleDelDraw(draw)"
            />
            <Message
              v-if="draw.interactingMethod === 4" :draw="draw"
              :image-urls="[draw.dynamicParams.base_image_url, draw.dynamicParams.ref_image_url]" :inversion="true"
              type="text-image" @delete="handleDelDraw(draw)" @set-public="handleSetPublic"
              @open-detail="openDraw(draw)"
            />
            <Message
              :draw="draw" :loading="draw.uuid === drawStore.loadingUuid" :image-urls="draw.imageUrls"
              :inversion="false" type="image"
              @del-one-image="(fileUrl: string) => handleDelOneImage(draw.uuid, fileUrl)"
              @open-detail="openDraw(draw)"
            />
          </template>
        </div>
      </template>
    </div>
    <NModal v-model:show="showDetailModal" preset="card" style="width: 95%" :bordered="true">
      <DrawDetail from-page-type="mineInChat" :draw-uuid="selectedDraw.uuid" @draw-deleted="handleDeleted" @hide="showDetailModal = false" />
    </NModal>
  </div>
</template>
