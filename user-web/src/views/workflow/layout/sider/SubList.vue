<script setup lang='ts'>
import { onActivated, onUnmounted, ref, watch } from 'vue'
import { NIcon } from 'naive-ui'
import { Cloud32Regular, LockClosed32Regular } from '@vicons/fluent'
import { useWfStore } from '@/store'
import { SvgIcon } from '@/components/common'
import { useScroll } from '@/views/chat/hooks/useScroll'
import { useBasicLayout } from '@/hooks/useBasicLayout'

const props = defineProps<Props>()
const wfStore = useWfStore()
const { scrollRef, scrollTo, scrollToTop } = useScroll()
const mouseEnterKbUuid = ref<string>('')
const { isMobile } = useBasicLayout()

interface Props {
  list: Workflow.WorkflowInfo[]
  activeWfUuid: string
}
async function handleSelect({ uuid }: Workflow.WorkflowInfo) {
  if (props.activeWfUuid === uuid)
    return
  wfStore.setActiveAndGo(uuid)
}

async function handleScroll(event: any) {
  const scrollTop = event.target.scrollTop
  localStorage.setItem('wfubListScrollPosition', scrollTop)
}

function handleMouseEnter({ uuid }: Workflow.WorkflowInfo) {
  mouseEnterKbUuid.value = uuid
}

function handleMouseLeave() {
  mouseEnterKbUuid.value = ''
}

watch(() => wfStore.myWorkflows, (newVal, oldVal) => {
  if (newVal.length > oldVal.length)
    scrollToTop()
})

onActivated(async () => {
  const savedPosition = localStorage.getItem('wfubListScrollPosition')
  if (savedPosition)
    scrollTo(savedPosition as unknown as number)
})

onUnmounted(() => {
  // 组件卸载前，可以清除之前保存的滚动位置
  localStorage.removeItem('wfubListScrollPosition')
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
          :class="item.uuid === activeWfUuid && ['border-[#4b9e5f]', 'bg-neutral-100', 'text-[#4b9e5f]', 'dark:bg-[#24272e]', 'dark:border-[#4b9e5f]', 'pr-14']"
          @click="handleSelect(item)" @mouseenter="handleMouseEnter(item)" @mouseleave="handleMouseLeave"
        >
          <span>
            <NIcon v-if="item.isPublic" :component="Cloud32Regular" />
            <NIcon v-if="!item.isPublic" :component="LockClosed32Regular" />
          </span>
          <div class="relative flex-1 overflow-hidden break-all text-ellipsis whitespace-nowrap">
            <span>{{ item.title }}</span>
          </div>
          <div v-if="mouseEnterKbUuid === item.uuid || isMobile" class="absolute z-10 flex visible right-1 pd-2">
            <button class="p-1">
              <SvgIcon
                :icon="(!item.isPublic) ? 'carbon:edit' : 'carbon:information'"
                @click.stop="wfStore.setShowCreateView(true, item.uuid)"
              />
            </button>
          </div>
        </a>
      </div>
    </template>
  </div>
</template>
