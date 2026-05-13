<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { useLoadingBar, useMessage } from 'naive-ui'
import DrawList from './DrawList.vue'
import Header from './Header.vue'
import { useGalleryStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'
import { debounce } from '@/utils/functions/debounce'

const ms = useMessage()
const loaddingBar = useLoadingBar()
const publicViewRef = ref()
const favViewRef = ref()
const galleryStore = useGalleryStore()
const loadingPublic = ref<boolean>(false)
const loadedPublicAll = ref<boolean>(false)
const loadingStarredList = ref<boolean>(false)
const loadedFavAll = ref<boolean>(false)
const nextPageMaxIdByPublic = ref<number>(Number.MAX_SAFE_INTEGER)
const nextPageMaxIdByStar = ref<number>(Number.MAX_SAFE_INTEGER)
const publicOrFavor = ref<string>('publicView')

/**
 * 加载公开列表
 */
async function loadNextPublicPage() {
  if (loadingPublic.value)
    return

  if (loadedPublicAll.value)
    return

  loaddingBar.start()
  loadingPublic.value = true
  try {
    const { data } = await api.fetchPublicDraws<Chat.DrawListResp>(nextPageMaxIdByPublic.value, 20)
    if (data.draws.length > 0) {
      nextPageMaxIdByPublic.value = data.minId
      galleryStore.appendPublicDraws(data.draws)
    } else {
      loadedPublicAll.value = true
      ms.warning(t('common.noMore'), {
        duration: 3000,
      })
    }
  } catch (error) {
    console.error(error)
  } finally {
    loadingPublic.value = false
    loaddingBar.finish()
  }
}

/**
 * 加载我的点赞
 */
async function loadNextStarPage() {
  if (loadingStarredList.value)
    return

  if (loadedFavAll.value)
    return

  loaddingBar.start()
  loadingStarredList.value = true
  try {
    const { data } = await api.fetchStarDraws<Chat.DrawListResp>(nextPageMaxIdByStar.value, 20)
    if (data.draws.length > 0) {
      nextPageMaxIdByStar.value = data.minId
      galleryStore.appendStarDraws(data.draws)
    } else {
      loadedFavAll.value = true
      ms.warning(t('common.noMore'), {
        duration: 3000,
      })
    }
  } catch (error) {
    console.error(error)
  } finally {
    loadingStarredList.value = false
    loaddingBar.finish()
  }
}

const handleLoadMorePublicDraws = debounce(loadNextPublicPage, 300)
const handleLoadMoreStarDraw = debounce(loadNextStarPage, 300)

function handleDisplayChange(value: string) {
  publicOrFavor.value = value
  if (publicOrFavor.value === 'favView' && galleryStore.myStarDraws.length === 0)
    handleLoadMoreStarDraw()
}

onMounted(() => {
  if (galleryStore.publicDraws.length === 0)
    handleLoadMorePublicDraws()
})
</script>

<template>
  <div class="flex flex-col w-full h-full">
    <Header @display-change="handleDisplayChange" />
    <main class="flex-1 overflow-hidden">
      <DrawList
        v-show="publicOrFavor === 'publicView'" ref="publicViewRef" from-page-type="public"
        :draws="galleryStore.publicDraws" @load-more="handleLoadMorePublicDraws"
      />
      <DrawList
        v-show="publicOrFavor === 'favView'" ref="favViewRef" from-page-type="starred"
        :draws="galleryStore.myStarDraws" :login-btn-enable="true" @load-more="handleLoadMoreStarDraw"
      />
    </main>
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
