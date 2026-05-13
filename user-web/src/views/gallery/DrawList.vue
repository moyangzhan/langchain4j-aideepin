<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { NModal } from 'naive-ui'
import CommonDraws from '@/views/draw/components/CommonDraws.vue'
import DrawDetail from '@/views/draw/components/DrawDetail.vue'
import { emptyDraw } from '@/utils/functions'

withDefaults(defineProps<Props>(), {
  draws: () => [],
})
const emit = defineEmits<Emit>()
const modalMainHeight = ref<number>(600)
const showDetailModal = ref<boolean>(false)
const selectedDraw = ref<Chat.Draw>(emptyDraw())

interface Emit {
  (e: 'loadMore', callback: Function): void
}

interface Props {
  draws: Chat.Draw[]
  showDelBtn?: boolean
  loginBtnEnable?: boolean
  fromPageType: string
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
  modalMainHeight.value = (window.innerHeight - 110)
})
</script>

<template>
  <div class="h-full overflow-y-auto">
    <CommonDraws :draws="draws" :login-btn-enable="true" @click-draw="openDraw" @load-more="loadMore" />
    <NModal v-model:show="showDetailModal" preset="card" style="width:95%" :bordered="true">
      <DrawDetail :from-page-type="fromPageType" :draw-uuid="selectedDraw.uuid" @draw-deleted="handleDelDraw" @hide="showDetailModal = false" />
    </NModal>
  </div>
</template>
