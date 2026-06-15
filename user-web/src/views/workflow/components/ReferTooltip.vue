<script setup lang="ts">
import { NTooltip } from 'naive-ui'
import { SvgIcon } from '@/components/common'
import { t } from '@/locales'
interface Props {
  brief?: boolean
}
withDefaults(defineProps<Props>(), {
  brief: false,
})

// 这些花括号是给用户看的教学示例（不是 i18n 插值占位符），用 self-reference 让插值把它们还原成字面文本
const literalBraces = {
  var_name1: '{var_name1}',
  input: '{input}',
  x: '{x}',
  y: '{y}',
}
</script>

<template>
  <NTooltip trigger="hover">
    <template #trigger>
      <SvgIcon icon="tabler:variable" class="text-base text-gray-500" />
    </template>
    <div v-if="!brief" class="flex flex-col pl-2 my-1">
      <div>{{ t('workflow.variableAvailable') }}</div>
      <div>{{ t('workflow.variableUsage') }}</div>
      <div>{{ t('workflow.variableUsage1', literalBraces) }}</div>
      <div>{{ t('workflow.variableUsage2', literalBraces) }}</div>
    </div>
    <div v-if="brief">
      {{ t('workflow.variableExample', literalBraces) }}
    </div>
  </NTooltip>
</template>
