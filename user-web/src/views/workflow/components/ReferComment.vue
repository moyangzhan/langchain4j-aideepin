<script setup lang="ts">
import { t } from '@/locales'

// 这些花括号是给用户看的教学示例（不是 i18n 插值占位符），用 self-reference 让插值把它们还原成字面文本
const literalBraces = {
  var_name1: '{var_name1}',
  input: '{input}',
}
</script>

<template>
  <div class="flex flex-col pl-2 my-1 text-xs text-gray-500">
    <div>{{ t('workflow.variableUsage1', literalBraces) }}</div>
    <div>{{ t('workflow.variableUsage2', literalBraces) }}</div>
  </div>
</template>
