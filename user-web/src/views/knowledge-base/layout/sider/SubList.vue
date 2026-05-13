<script setup lang='ts'>
import { onActivated, onUnmounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { NButton, NIcon } from 'naive-ui'
import { Cloud32Regular, LockClosed32Regular } from '@vicons/fluent'
import { useKbStore } from '@/store'
import { SvgIcon } from '@/components/common'
import { useScroll } from '@/views/chat/hooks/useScroll'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import KbInfo from '@/views/knowledge-base/Header/KbInfo.vue'
const props = defineProps<Props>()
const router = useRouter()
const kbStore = useKbStore()
const { scrollRef, scrollTo } = useScroll()
const { isMobile } = useBasicLayout()
const mouseEnterKbUuid = ref<string>('')
const showModal = ref<boolean>(false)
const tmpKb = ref<KnowledgeBase.Info>(knowledgeBaseEmptyInfo())

interface Props {
  list: KnowledgeBase.Info[]
  activeKbUuid: string
}
async function handleSelect({ uuid }: KnowledgeBase.Info) {
  if (props.activeKbUuid === uuid)
    return
  kbStore.setActive(uuid)
  router.replace({ name: 'QADetail', params: { kbUuid: uuid } })
}
async function handleScroll(event: any) {
  const scrollTop = event.target.scrollTop
  localStorage.setItem('subListScrollPosition', scrollTop)
}
function handleMouseEnter({ uuid }: KnowledgeBase.Info) {
  mouseEnterKbUuid.value = uuid
}
function handleMouseLeave() {
  mouseEnterKbUuid.value = ''
}
function showKb(item: KnowledgeBase.Info) {
  showModal.value = true
  tmpKb.value = item
}
onActivated(async () => {
  const savedPosition = localStorage.getItem('subListScrollPosition')
  if (savedPosition)
    scrollTo(savedPosition as unknown as number)
})
onUnmounted(() => {
  // 组件卸载前，可以清除之前保存的滚动位置
  localStorage.removeItem('subListScrollPosition')
})
</script>

<template>
  <div ref="scrollRef" class="px-4 h-full overflow-y-auto" @scroll="handleScroll">
    <template v-if="!list.length">
      <div class="flex flex-col items-center mt-4 text-center text-neutral-300">
        <SvgIcon icon="ri:inbox-line" class="mb-2 text-3xl" />
        <span>{{ $t('common.noData') }}</span>
      </div>
    </template>
    <template v-else>
      <div class="flex flex-col gap-2 text-sm">
        <a
          v-for="item of list" :key="item.uuid"
          class="relative flex items-center gap-3 px-3 py-3 break-all border rounded-md cursor-pointer hover:bg-neutral-100 group dark:border-neutral-800 dark:hover:bg-[#24272e]"
          :class="item.uuid === activeKbUuid && ['border-[#4b9e5f]', 'bg-neutral-100', 'text-[#4b9e5f]', 'dark:bg-[#24272e]', 'dark:border-[#4b9e5f]', 'pr-14']"
          @click="handleSelect(item)" @mouseenter="handleMouseEnter(item)" @mouseleave="handleMouseLeave"
        >
          <span>
            <NIcon v-if="item.isPublic" :component="Cloud32Regular" />
            <NIcon v-if="!item.isPublic" :component="LockClosed32Regular" />
          </span>
          <div class="relative flex-1 overflow-hidden break-all text-ellipsis whitespace-nowrap">
            <span>{{ item.title }}</span>
          </div>
          <div class="absolute z-10 flex visible right-1">
            <NButton
              v-show="mouseEnterKbUuid === item.uuid || isMobile" secondary size="tiny"
              @click.stop="showKb(item)"
            >
              <SvgIcon icon="si:align-left-detailed-line" />
            </NButton>
          </div>
        </a>
      </div>
    </template>
  </div>

  <KbInfo :show-modal="showModal" :knowledge-base="tmpKb" @show-modal="showModal = $event" />
</template>
