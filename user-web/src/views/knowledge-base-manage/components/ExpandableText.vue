<script setup lang="ts">
import { nextTick, onBeforeUnmount, onMounted, ref } from 'vue'
import { NButton } from 'naive-ui'
import { useI18n } from 'vue-i18n'

const props = defineProps<{
  text: string
  lines?: number
}>()

const { t } = useI18n()
const collapsed = ref(true)
const overflowing = ref(false)
const textEl = ref<HTMLElement | null>(null)
let resizeObserver: ResizeObserver | null = null

function measure() {
  const el = textEl.value
  if (!el)
    return
  overflowing.value = collapsed.value && el.scrollHeight > el.clientHeight + 1
}

onMounted(() => {
  measure()
  if (typeof ResizeObserver !== 'undefined' && textEl.value) {
    resizeObserver = new ResizeObserver(() => measure())
    resizeObserver.observe(textEl.value)
  }
})

onBeforeUnmount(() => {
  resizeObserver?.disconnect()
})

function toggle() {
  collapsed.value = !collapsed.value
  nextTick(measure)
}
</script>

<script lang="ts">
export default { name: 'ExpandableText' }
</script>

<template>
  <div>
    <div
      ref="textEl" class="expandable-text" :class="{ 'expandable-text-clamped': collapsed }"
      :style="{ '-webkit-line-clamp': props.lines ?? 3 }"
    >
      {{ props.text }}
    </div>
    <NButton v-if="overflowing && collapsed" text type="primary" size="tiny" @click="toggle">
      {{ t('common.expand') }}
    </NButton>
    <NButton v-else-if="!collapsed" text type="primary" size="tiny" @click="toggle">
      {{ t('common.collapse') }}
    </NButton>
  </div>
</template>

<style scoped>
.expandable-text {
  white-space: pre-wrap;
  word-break: break-all;
}
.expandable-text-clamped {
  display: -webkit-box;
  -webkit-box-orient: vertical;
  overflow: hidden;
}
</style>
