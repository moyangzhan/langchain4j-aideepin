<script lang="ts" setup>
import { ref } from 'vue'
import EditConv from './EditConv.vue'
import { HoverButton, SvgIcon } from '@/components/common'
import { useAuthStore } from '@/store'
import { emptyConv } from '@/utils/functions'

interface Props {
  conversation: Chat.Conversation
}
withDefaults(defineProps<Props>(), {
  conversation: () => emptyConv(),
})
const showEditModal = ref(false)
const showEditBtn = ref(false)
const authStore = useAuthStore()

function openEditView() {
  if (!authStore.checkLoginOrShow())
    return
  showEditModal.value = true
}
function showOrCloseModal(show: boolean) {
  showEditModal.value = show
}
</script>

<template>
  <header
    class="sticky top-0 left-0 right-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur"
    @mouseenter="() => showEditBtn = true" @mouseleave="() => showEditBtn = false"
  >
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-12">
      <div class="flex items-center flex-col mx-2">
        <p class="text-sm">
          {{ conversation?.title ?? '' }}
        </p>
      </div>
      <div v-show="showEditBtn" class="flex items-center space-x-2">
        <HoverButton @click="openEditView()">
          <span class="text-xl">
            <SvgIcon icon="carbon:edit" />
          </span>
        </HoverButton>
      </div>
    </div>
  </header>
  <EditConv :show-modal="showEditModal" :conversation="conversation" @showModal="showOrCloseModal" />
</template>
