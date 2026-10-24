<script setup lang='ts'>
import { NIcon, NCollapseTransition } from 'naive-ui'
import { ChevronDown16Filled, ChevronUp16Filled } from '@vicons/fluent'
import { ref } from 'vue'

const expanded = ref<boolean>(true)
</script>

<template>
  <div class="relative">
    <div
      class="absolute bottom-0 right-2 z-10 -translate-y-1/2 cursor-pointer select-none"
      @click="expanded = !expanded"
    >
      <span class="flex items-center rounded bg-neutral-100 px-2 py-0.5 text-neutral-500 transition-colors hover:bg-neutral-200 hover:text-neutral-700 dark:bg-neutral-800 dark:text-neutral-400 dark:hover:bg-neutral-700 dark:hover:text-neutral-200">
        <NIcon size="16">
          <ChevronDown16Filled v-if="expanded" />
          <ChevronUp16Filled v-else />
        </NIcon>
      </span>
    </div>
    <NCollapseTransition :show="expanded">
      <slot />
    </NCollapseTransition>
  </div>
</template>
