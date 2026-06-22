<script setup lang='ts'>
import { computed, nextTick, onActivated, ref, watch } from 'vue'
import { useLoadingBar, useMessage } from 'naive-ui'
import DisplayStyleInChat from './components/DisplayStyleInChat.vue'
import DisplayStyleInGallery from './components/DisplayStyleInGallery.vue'
import GptImageEditor from './components/gpt-image/GptImageEditor.vue'
import Wanx from './components/wanx/index.vue'
import Siliconflow from './components/siliconflow/index.vue'
import Header from './components/Header.vue'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAppStore, useAuthStore, useDrawStore } from '@/store'
import api from '@/api'
import { debounce } from '@/utils/functions/debounce'
import { t } from '@/locales'
import { changeFileUrlToUuid } from '@/utils/functions'
const appStore = useAppStore()
const ms = useMessage()
const loaddingBar = useLoadingBar()
const { isMobile } = useBasicLayout()
const authStore = useAuthStore()
const drawStore = useDrawStore()
const chatStyleViewRef = ref()
const galleryStyleViewRef = ref()
const loading = ref<boolean>(false)
const loadedAll = ref<boolean>(false)
const nextPageMaxImageId = ref<number>(Number.MAX_SAFE_INTEGER)
const selectedDisplayStyle = ref<string>('chatStyle')

async function loadNextPage(callback: Function) {
  if (loading.value)
    return

  if (loadedAll.value)
    return

  loaddingBar.start()
  loading.value = true
  try {
    const { data } = await api.fetchDraws<Chat.DrawListResp>(nextPageMaxImageId.value, 20)
    if (data.draws.length > 0) {
      nextPageMaxImageId.value = data.minId
      drawStore.unshiftDraws(data.draws)
    } else {
      loadedAll.value = true
      ms.warning(t('common.noMore'), {
        duration: 3000,
      })
    }

    callback()
    console.log('draw loadNextPage')
  } catch (error) {
    console.error(error)
  } finally {
    loading.value = false
    loaddingBar.finish()
  }
}

const handleLoadMoreDraws = debounce(loadNextPage, 300)

/**
 * 删除作图任务中的一张图片
 */
async function handleDelOneImage(uuid: string, fileUrl: string) {
  if (loading.value)
    return
  const fileUuid = changeFileUrlToUuid(fileUrl)
  const ret = await api.drawFileDel<boolean>(uuid, fileUuid)
  if (ret)
    drawStore.deleteOneFile(uuid, fileUuid)
}

const footerClass = computed(() => {
  let classes = ['p-4']
  if (isMobile.value)
    classes = ['sticky', 'left-0', 'bottom-0', 'right-0', 'p-2', 'pr-3', 'overflow-hidden']
  return classes
})

function submitted() {
  nextTick(() => {
    chatStyleViewRef.value.gotoBottom()
    galleryStyleViewRef.value.gotoTop()
  })
}

function onDisplayStyleChange(value: string) {
  selectedDisplayStyle.value = value
}

watch(
  () => authStore.token,
  () => {
    if (authStore.token) {
      console.log('draw first load', authStore.token)
      handleLoadMoreDraws(() => {
        chatStyleViewRef.value.gotoBottom()
        galleryStyleViewRef.value.gotoTop()
      })
    }
  },
  { immediate: true },
)

onActivated(async () => {
  chatStyleViewRef.value.gotoBottom()
  galleryStyleViewRef.value.gotoTop()
})
</script>

<template>
  <div class="flex flex-col w-full h-full">
    <Header @display-style-change="onDisplayStyleChange" />
    <main class="flex-1 overflow-hidden">
      <DisplayStyleInChat
        v-show="selectedDisplayStyle === 'chatStyle'" ref="chatStyleViewRef"
        @del-one-image="handleDelOneImage" @load-more="handleLoadMoreDraws"
      />
      <DisplayStyleInGallery
        v-show="selectedDisplayStyle === 'galleryStyle'" ref="galleryStyleViewRef"
        :draws="drawStore.imagesOrderByIdDesc" @load-more="handleLoadMoreDraws"
      />
    </main>
    <footer :class="footerClass">
      <div class="w-full max-w-screen-xl m-auto">
        <GptImageEditor v-show="appStore.selectedImageModel.modelPlatform === 'openai'" @submitted="submitted" />
        <Wanx v-show="appStore.selectedImageModel.modelPlatform === 'dashscope'" @submitted="submitted" />
        <Siliconflow v-show="appStore.selectedImageModel.modelPlatform === 'siliconflow'" @submitted="submitted" />
      </div>
    </footer>
  </div>
</template>

<style>
.collapse-enter-active,
.collapse-leave-active {
  transition: height 0.5s ease;
}

.collapse-enter,
.collapse-leave-to {
  height: 0;
  overflow: hidden;
}
</style>
