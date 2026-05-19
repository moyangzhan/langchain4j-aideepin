<script lang="ts" setup>
import { ref } from 'vue'
import EditConv from './EditConv.vue'
import { HoverButton, SvgIcon, ApiKeyModal } from '@/components/common'
import { useAuthStore } from '@/store'
import { emptyCharacter } from '@/utils/functions'

interface Props {
  character: Chat.Character
}
withDefaults(defineProps<Props>(), {
  character: () => emptyCharacter(),
})
const showEditModal = ref(false)
const showEditBtn = ref(false)
const showApiKeyModal = ref(false)
const authStore = useAuthStore()

function openEditView() {
  if (!authStore.checkLoginOrShow())
    return
  showEditModal.value = true
}
function showOrCloseModal(show: boolean) {
  showEditModal.value = show
}
function extApiKey() {
  if (!authStore.checkLoginOrShow())
    return
  showApiKeyModal.value = true
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
          {{ character?.title ?? '' }}
        </p>
      </div>
      <div v-show="showEditBtn" class="flex items-center space-x-2">
        <HoverButton @click="extApiKey()">
          <span class="text-xl">
            <SvgIcon icon="carbon:api" />
          </span>
        </HoverButton>
        <HoverButton @click="openEditView()">
          <span class="text-xl">
            <SvgIcon icon="carbon:edit" />
          </span>
        </HoverButton>
      </div>
    </div>
  </header>
  <EditConv :show-modal="showEditModal" :character="character" @showModal="showOrCloseModal" />
  <ApiKeyModal v-model:show="showApiKeyModal" type="character" :uuid="character.uuid" :title="character.title" />
</template>
