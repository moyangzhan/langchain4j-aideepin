<script lang="ts" setup>
import { ref } from 'vue'
import { HoverButton, SvgIcon } from '@/components/common'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import KbInfo from '@/views/knowledge-base/Header/KbInfo.vue'

interface Props {
  knowledgeBase: KnowledgeBase.Info
}
withDefaults(defineProps<Props>(), {
  knowledgeBase: () => knowledgeBaseEmptyInfo(),
})
const showEditModal = ref(false)

function openEditView() {
  console.log('openEditView', showEditModal.value)
  showEditModal.value = true
}
function showOrCloseModal(show: boolean) {
  showEditModal.value = show
}
</script>

<template>
  <header
    class="left-0 top-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur"
  >
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-10">
      <div class="flex items-center flex-col mx-2">
        <p class="text-sm">
          {{ knowledgeBase?.title ?? '' }}
        </p>
      </div>
      <div class="flex items-center space-x-2">
        <HoverButton @click="openEditView()">
          <span class="text-xl">
            <SvgIcon icon="si:align-left-detailed-line" />
          </span>
        </HoverButton>
      </div>
    </div>
    <KbInfo v-if="knowledgeBase && knowledgeBase.uuid" :show-modal="showEditModal" :knowledge-base="knowledgeBase" @showModal="showOrCloseModal" />
  </header>
</template>
