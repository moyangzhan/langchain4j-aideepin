<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { NModal } from 'naive-ui'
import DrawDetail from './DrawDetail.vue'
import CommonDraws from './CommonDraws.vue'
import { emptyDraw } from '@/utils/functions'

withDefaults(defineProps<Props>(), {
  draws: () => [],
  showDelBtn: true,
})
const emit = defineEmits<Emit>()
const commonDrawsRef = ref()
const modalMainHeight = ref<number>(600)
const showDetailModal = ref<boolean>(false)
const selectedDraw = ref<Chat.Draw>(emptyDraw())

interface Emit {
  (e: 'loadMore', callback: Function): void
}

interface Props {
  draws: Chat.Draw[]
}

function loadMore() {
  emit('loadMore', () => {

  })
}

function openDraw(item: Chat.Draw) {
  showDetailModal.value = true
  selectedDraw.value = item
}

function handleDelDraw(uuid: string, prompt: string) {
  showDetailModal.value = false
}

onMounted(() => {
  modalMainHeight.value = (window.innerHeight - 100)
})

function gotoTop() {
  commonDrawsRef.value.gotoTop()
}

function gotoBottom() {
  commonDrawsRef.value.gotoBottom()
}
defineExpose({ gotoTop, gotoBottom })
</script>

<template>
  <div class="h-full overflow-y-auto">
    <CommonDraws ref="commonDrawsRef" :draws="draws" :login-btn-enable="true" @click-draw="openDraw" @load-more="loadMore" />
    <NModal v-model:show="showDetailModal" preset="card" style="width:95%" :bordered="true">
      <DrawDetail from-page-type="mineInGallery" :draw-uuid="selectedDraw.uuid" @draw-deleted="handleDelDraw" @hide="showDetailModal = false" />
    </NModal>
  </div>
</template>
