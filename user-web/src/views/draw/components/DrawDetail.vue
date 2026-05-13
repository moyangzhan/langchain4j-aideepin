<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NAvatar, NButton, NCard, NCarousel, NEmpty, NFlex, NFloatButton, NIcon, NImage, NImageGroup, NInput, NList, NListItem, NPagination, NSpin, NTag, NThing, NTooltip, useDialog, useLoadingBar, useMessage } from 'naive-ui'
import { useRouter } from 'vue-router'
import { ArrowDown, ArrowUp, Reload } from '@vicons/ionicons5'
import DrawDetailFuncBar from './DrawDetailFuncBar.vue'
import { useAuthStore, useDrawStore, useGalleryStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import NoPic from '@/assets/no_pic.png'
import { calcImageUrls, emptyDraw } from '@/utils/functions'
import api from '@/api'
import { t } from '@/locales'

const props = withDefaults(defineProps<Props>(), {
  drawUuid: '',
  fromPageType: '',
})
const emit = defineEmits<Emit>()
const router = useRouter()
const dialog = useDialog()
const drawStore = useDrawStore()
const authStore = useAuthStore()
const galleryStore = useGalleryStore()
const ms = useMessage()
const { isMobile } = useBasicLayout()
const loading = ref<boolean>(false)
const loadingBar = useLoadingBar()
const currentDrawUuid = ref<string>('')
const currentDraw = ref<Chat.Draw>(emptyDraw())
const comments = ref<Chat.DrawComment[]>([])
const currentPage = ref<number>(1)
const totalPage = ref<number>(0)
const defaultImageHeight = ref<number>(600)
const newComment = ref<string>('')
const submitting = ref<boolean>(false)

interface Props {
  drawUuid: string
  fromPageType: string
}
interface Emit {
  (e: 'drawDeleted', uuid: string, prompt: string): void
  (e: 'hide'): void
}
function handleDelDraw(uuid: string, prompt: string) {
  dialog.warning({
    title: `${t('draw.deleteDrawConfirm')}${prompt.substring(0, 11)}]?`,
    content: t('draw.deleteDrawContent'),
    positiveText: t('common.yes'),
    negativeText: t('common.no'),
    onPositiveClick: async () => {
      await api.drawDel<boolean>(uuid)
      drawStore.deleteDraw(uuid)
      emit('drawDeleted', uuid, prompt)
    },
  })
}

async function handleSetPublic(uuid: string, isPublic: boolean) {
  const ret = await api.drawSetPublic<Chat.Draw>(uuid, isPublic)
  if (ret) {
    calcImageUrls(ret.data)
    drawStore.setPublic(uuid, isPublic)
    galleryStore.setPublic(ret.data)
    currentDraw.value.isPublic = ret.data.isPublic
    ms.warning(t('draw.publicAccessChanged', { status: isPublic ? t('draw.publicAccessEnabled') : t('draw.publicAccessDisabled') }))
  }
}

async function fetchDrawInfo() {
  if (loading.value || !currentDrawUuid.value)
    return

  loading.value = true
  loadingBar.start()
  try {
    const res = await api.fetchDraw<Chat.Draw>(currentDrawUuid.value)
    if (res.data)
      assignNewToCurrentDraw(res.data)
  } finally {
    loading.value = false
    loadingBar.finish()
  }
}

async function fetchComments() {
  const { data } = await api.fetchDrawComments<PageResponse>(currentDrawUuid.value, 1, 10)
  if (data) {
    comments.value = data.records
    currentPage.value = data.current
    totalPage.value = data.pages
  }
}

async function fetchPrev() {
  console.log('fetch prev', currentDrawUuid.value)
  try {
    let resp
    switch (props.fromPageType) {
      case 'mineInChat':
        resp = await api.fetchOlderMineDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'mineInGallery':
        resp = await api.fetchNewerMineDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'starred':
        resp = await api.fetchNewerStarredDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'public':
        resp = await api.fetchNewerPublicDraw<Chat.Draw>(currentDrawUuid.value)
        break
    }
    if (resp && resp.data)
      assignNewToCurrentDraw(resp.data)
    else
      ms.info(t('draw.noPrevious'))
  } catch (e) {
    console.error(e)
  }
}

async function fetchNext() {
  console.log('fetch next', currentDrawUuid.value)
  try {
    let resp
    switch (props.fromPageType) {
      case 'mineInChat':
        resp = await api.fetchNewerMineDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'mineInGallery':
        resp = await api.fetchOlderMineDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'starred':
        resp = await api.fetchOlderStarredDraw<Chat.Draw>(currentDrawUuid.value)
        break
      case 'public':
        resp = await api.fetchOlderPublicDraw<Chat.Draw>(currentDrawUuid.value)
        break
    }
    if (resp && resp.data)
      assignNewToCurrentDraw(resp.data)
    else
      ms.info(t('draw.noNext'))
  } catch (e) {
    console.error(e)
  }
}

function assignNewToCurrentDraw(newDraw: Chat.Draw) {
  currentDraw.value = newDraw
  currentDrawUuid.value = currentDraw.value.uuid
  calcImageUrls(currentDraw.value)
  fetchComments()
}

async function handleSubmit() {
  if (!authStore.checkLoginOrShow())
    return

  if (submitting.value)
    return

  submitting.value = true
  try {
    const resp = await api.drawCommentAdd<Chat.DrawComment>(currentDraw.value.uuid, newComment.value)
    if (resp && resp.data)
      comments.value.push(resp.data)
  } catch (error: any) {
    ms.error(error.message)
  } finally {
    submitting.value = false
  }
}

function openDrawTab() {
  router.replace({
    name: 'Draw',
  })
  emit('hide')
}

const submitDisable = computed(() => newComment.value.trim().length < 3 || submitting)

onMounted(() => {
  defaultImageHeight.value = Math.max(window.innerHeight - 150, defaultImageHeight.value)
})

watch(
  () => props.drawUuid,
  (newVal) => {
    if (newVal) {
      currentDrawUuid.value = newVal
      fetchDrawInfo()
    }
  },
  { immediate: true },
)
</script>

<template>
  <div>
    <div class="flex flex-wrap">
      <NFlex justify="center" class="grow">
        <template v-if="loading">
          <NSpin size="medium">
            <template #icon>
              <NIcon>
                <Reload />
              </NIcon>
            </template>
          </NSpin>
        </template>
        <template v-else>
          <template v-if="currentDraw.uuid === drawStore.loadingUuid && currentDraw.processStatus === 2">
            <NEmpty :description="`${t('draw.errorPrefix')}${currentDraw.processStatusRemark}`" />
          </template>
          <template
            v-else-if="currentDraw.uuid !== drawStore.loadingUuid && (!currentDraw.imageUrls || currentDraw.imageUrls.length === 0)"
          >
            <NEmpty :description="t('draw.imageNotFound')" />
          </template>
          <template v-else-if="currentDraw.imageUuids && currentDraw.imageUuids.length > 0">
            <NImageGroup>
              <NFlex justify="center">
                <NCarousel dot-placement="bottom" :style="`height:${defaultImageHeight}px;max-width:1000px;`">
                  <template v-for="imageUrl in currentDraw.imageUrls" :key="imageUrl">
                    <NImage
                      v-if="imageUrl && currentDraw.uuid !== drawStore.loadingUuid"
                      :style="`height:${defaultImageHeight}px`" :src="`${imageUrl}?token=${authStore.token}`"
                      :fallback-src="NoPic" object-fit="cover"
                    />
                  </template>
                  <template #dots="{ total, currentIndex, to }">
                    <ul class="custom-dots">
                      <li
                        v-for="index of total" :key="index" :class="{ ['is-active']: currentIndex === index - 1 }"
                        @click="to(index - 1)"
                      />
                    </ul>
                  </template>
                </NCarousel>
              </NFlex>
            </NImageGroup>
          </template>
        </template>
      </NFlex>
      <div class="flex flex-col w-[400px]">
        <div>
          <DrawDetailFuncBar :draw="currentDraw" @set-public="handleSetPublic" @del-draw="handleDelDraw" />
        </div>
        <NCard :title="t('draw.comment')" size="small" class="mt-10">
          <NFlex vertical>
            <div :style="`max-height: ${defaultImageHeight / 2}px`" class="overflow-y-auto">
              <NList hoverable>
                <NListItem v-for="comment in comments" :key="comment.uuid">
                  <NThing content-style="margin-top: 10px;">
                    <template #description>
                      <NFlex style="margin-top: 4px" justify="space-between">
                        <NTag :bordered="false" :color="{ color: '#ff000000' }">
                          {{ comment.userName }}
                          <template #avatar>
                            <NAvatar :src="`/api/user/avatar/${comment.userUuid}`" size="large" color="#ff0000000" />
                          </template>
                        </NTag>
                        <div class="text-xs">
                          {{ comment.createTime }}
                        </div>
                      </NFlex>
                    </template>
                    {{ comment.remark }}
                  </NThing>
                </NListItem>
              </NList>
            </div>
            <NFlex justify="end">
              <NPagination
                v-show="comments.length > 0" v-model:page="currentPage" :page-count="totalPage"
                :page-slot="5"
              />
            </NFlex>
            <NInput
              v-model:value="newComment" class="mt-3" type="textarea" :placeholder="t('draw.commentPlaceholder')" :autosize="{
                minRows: 2,
              }"
            />
            <div class="flex justify-between">
              <NButton type="primary" text tag="a" target="_blank" :disable="submitDisable" @click="openDrawTab">
                {{ t('draw.drawSame') }}
              </NButton>
              <NButton type="primary" ghost :disable="submitDisable" @click="handleSubmit">
                {{ t('common.submit') }}
              </NButton>
            </div>
          </NFlex>
        </NCard>
      </div>
    </div>
    <NTooltip trigger="hover" placement="left">
      <template #trigger>
        <NFloatButton
          :right="isMobile ? 15 : 490" :top="defaultImageHeight / 2" position="fixed"
          style="border:1px solid lightgray; --n-box-shadow: 0 0px 1px 0px black;" @click="fetchPrev"
        >
          <NIcon>
            <ArrowUp style="color:gray" />
          </NIcon>
        </NFloatButton>
      </template>
      {{ t('draw.previousDraw') }}
    </NTooltip>
    <NTooltip trigger="hover" placement="left">
      <template #trigger>
        <NFloatButton
          :right="isMobile ? 15 : 490" :top="defaultImageHeight / 2 + 50" position="fixed"
          style="border:1px solid lightgray; --n-box-shadow: 0 0px 1px 0px black;" @click="fetchNext"
        >
          <NIcon>
            <ArrowDown style="color:gray" />
          </NIcon>
        </NFloatButton>
      </template>
      {{ t('draw.nextDraw') }}
    </NTooltip>
  </div>
</template>

<style scoped>
.custom-dots {
  display: flex;
  margin: 0;
  padding: 0;
  position: absolute;
  bottom: 5px;
  left: 20px;
  padding-top: 15px;
  padding-bottom: 15px;
}

.custom-dots li {
  display: inline-block;
  width: 12px;
  height: 8px;
  margin: 0 3px;
  border-width: 1px;
  border-radius: 4px;
  border-color: lightgrey;
  background-color: #fff;
  transition:
    width 0.3s,
    background-color 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  cursor: pointer;
}

.custom-dots li.is-active {
  width: 40px;
  border-color: lightgrey;
  background: #fff;
}
</style>
