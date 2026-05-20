<script lang="ts" setup>
import { h, ref } from 'vue'
import { NDropdown } from 'naive-ui'
import { SvgIcon, ApiKeyModal } from '@/components/common'
import { useAuthStore } from '@/store'
import { t } from '@/locales'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import KbInfo from '@/views/knowledge-base/Header/KbInfo.vue'

interface Props {
  knowledgeBase: KnowledgeBase.Info
}
withDefaults(defineProps<Props>(), {
  knowledgeBase: () => knowledgeBaseEmptyInfo(),
})
const showEditModal = ref(false)
const showApiKeyModal = ref(false)
const authStore = useAuthStore()

const options = [
  {
    label: t('common.detail'),
    key: 'detail',
    icon: () => h(SvgIcon, { icon: 'carbon:information', class: 'text-base cursor-pointer' }),
  },
  {
    label: 'API',
    key: 'api',
    icon: () => h(SvgIcon, { icon: 'carbon:api', class: 'text-base cursor-pointer' }),
  },
]

function handleSelect(key: string) {
  if (!authStore.checkLoginOrShow())
    return
  if (key === 'detail')
    showEditModal.value = true
  else if (key === 'api')
    showApiKeyModal.value = true
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
      <NDropdown :options="options" @select="handleSelect">
        <SvgIcon class="w-6 cursor-pointer" icon="ri:more-fill" />
      </NDropdown>
    </div>
    <KbInfo v-if="knowledgeBase && knowledgeBase.uuid" :show-modal="showEditModal" :knowledge-base="knowledgeBase" @showModal="showOrCloseModal" />
    <ApiKeyModal v-if="knowledgeBase" v-model:show="showApiKeyModal" type="knowledge" :uuid="knowledgeBase.uuid" :title="knowledgeBase.title" />
  </header>
</template>
