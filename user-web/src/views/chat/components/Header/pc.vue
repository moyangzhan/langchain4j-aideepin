<script lang="ts" setup>
import { h, ref } from 'vue'
import { NButton, NDropdown } from 'naive-ui'
import EditConv from './EditConv.vue'
import { ApiKeyModal, SvgIcon } from '@/components/common'
import { useAuthStore } from '@/store'
import { t } from '@/locales'
import { emptyCharacter } from '@/utils/functions'

interface Props {
  character: Chat.Character
}
withDefaults(defineProps<Props>(), {
  character: () => emptyCharacter(),
})
const showEditModal = ref(false)

const showApiKeyModal = ref(false)
const authStore = useAuthStore()

const options = [
  {
    label: t('common.edit'),
    key: 'edit',
    icon: () => h(SvgIcon, { icon: 'carbon:edit', class: 'text-base cursor-pointer' }),
  },
  {
    label: t('extApi.apiAccess'),
    key: 'api',
    icon: () => h(SvgIcon, { icon: 'carbon:api', class: 'text-base cursor-pointer' }),
  },
]

function handleSelect(key: string) {
  if (key === 'edit') {
    if (!authStore.checkLoginOrShow())
      return
    showEditModal.value = true
  } else if (key === 'api') {
    showApiKeyModal.value = true
  }
}
function showOrCloseModal(show: boolean) {
  showEditModal.value = show
}
</script>

<template>
  <header
    class="sticky top-0 left-0 right-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur"
  >
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-12">
      <div class="flex items-center flex-col mx-2">
        <p class="text-sm">
          {{ character?.title ?? '' }}
        </p>
      </div>
      <NDropdown :options="options" @select="handleSelect">
        <NButton quaternary circle size="small">
          <template #icon>
            <SvgIcon icon="ri:more-fill" />
          </template>
        </NButton>
      </NDropdown>
    </div>
  </header>
  <EditConv v-if="character" :show-modal="showEditModal" :character="character" @showModal="showOrCloseModal" />
  <ApiKeyModal v-if="character" v-model:show="showApiKeyModal" type="character" :uuid="character.uuid" :title="character.title" />
</template>
