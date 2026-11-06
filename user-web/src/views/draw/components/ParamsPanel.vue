<script setup lang='ts'>
import { NIcon } from 'naive-ui'
import { ChevronDown16Filled, ChevronUp16Filled } from '@vicons/fluent'
import { ref } from 'vue'

const expanded = ref<boolean>(true)
</script>

<template>
  <div class="relative min-h-8">
    <div
      class="absolute top-1 right-2 z-10 cursor-pointer select-none"
      @click="expanded = !expanded"
    >
      <span class="flex items-center rounded bg-neutral-100 px-2 py-0.5 text-neutral-500 transition-colors hover:bg-neutral-200 hover:text-neutral-700 dark:bg-neutral-800 dark:text-neutral-400 dark:hover:bg-neutral-700 dark:hover:text-neutral-200">
        <NIcon size="16">
          <ChevronDown16Filled v-if="expanded" />
          <ChevronUp16Filled v-else />
        </NIcon>
      </span>
    </div>
    <!-- 折叠用 grid 0fr/1fr 而非卸载式过渡：子树须保持挂载，否则上传等子组件内部状态会丢失 -->
    <!-- Collapse via grid 0fr/1fr instead of unmounting: children stay mounted so in-child state (uploads) survives -->
    <div
      class="grid transition-[grid-template-rows] duration-300 ease-in-out"
      :class="expanded ? 'grid-rows-[1fr]' : 'grid-rows-[0fr]'"
    >
      <div class="min-h-0 overflow-hidden">
        <slot />
      </div>
    </div>
  </div>
</template>
