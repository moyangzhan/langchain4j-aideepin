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
</script>

<template>
  <NTooltip trigger="hover">
    <template #trigger>
      <SvgIcon icon="tabler:variable" class="text-base text-gray-500" />
    </template>
    <div v-if="!brief" class="flex flex-col pl-2 my-1">
      <div>{{ t('workflow.variableAvailable') }}</div>
      <div>{{ t('workflow.variableUsage') }}</div>
      <div>{{ t('workflow.variableUsage1') }}</div>
      <div>{{ t('workflow.variableUsage2') }}</div>
    </div>
    <div v-if="brief">
      {{ t('workflow.variableExample') }}
    </div>
  </NTooltip>
</template>
