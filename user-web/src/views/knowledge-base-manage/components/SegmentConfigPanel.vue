<script setup lang='ts'>
import { computed } from 'vue'
import { RouterLink } from 'vue-router'
import { SPLIT_STRATEGY } from '@/utils/constant'
import { t } from '@/locales'

const props = defineProps<{
  kb: KnowledgeBase.Info
  kbUuid: string
  segmentMode?: string
}>()

const segmentConfigRows = computed(() => {
  const tokenLabel = props.segmentMode === 'parent_child'
    ? t('knowledgeBase.parentChunkMaxLabel')
    : props.segmentMode === 'qa' ? t('knowledgeBase.qaChunkMaxLabel') : t('knowledgeBase.textChunkMaxLabel')
  const sharedNote = props.segmentMode === 'parent_child' ? t('knowledgeBase.strategySharedNote') : ''
  const strategyRow = props.kb.ingestSplitStrategy === 'custom'
    ? { label: t('knowledgeBase.customSeparator'), value: `${props.kb.ingestCustomSeparator}${sharedNote}` }
    : { label: t('knowledgeBase.configStrategyLabel'), value: `${SPLIT_STRATEGY.find(option => option.value === props.kb.ingestSplitStrategy)?.label ?? props.kb.ingestSplitStrategy ?? ''}${sharedNote}` }
  return [
    { label: t('knowledgeBase.ingestModelLabel'), value: props.kb.ingestModelName || '-' },
    { label: tokenLabel, value: `${props.kb.ingestMaxSegmentSize ?? 0}` },
    { label: t('knowledgeBase.configOverlapLabel'), value: `${props.kb.ingestMaxOverlap ?? 0}` },
    strategyRow,
  ]
})
</script>

<template>
  <div class="flex items-center gap-2" style="margin-top: 8px;">
    <span style="font-weight: 600;">{{ t('knowledgeBase.segmentConfigLabel') }}</span>
    <span style="font-size: 12px; opacity: 0.7;">
      {{ t('knowledgeBase.configFromKbPrefix') }}<RouterLink class="hljs-link" :to="{ name: 'KnowledgeBaseEdit', params: { kbUuid } }">{{ t('knowledgeBase.kbSettingsLink') }}</RouterLink>{{ t('knowledgeBase.configFromKbSuffix') }}
    </span>
  </div>
  <div class="flex flex-col" style="font-size: 12px; gap: 2px; padding-left: 4px;">
    <div v-for="row in segmentConfigRows" :key="row.label">
      <span>{{ row.label }}: {{ row.value }}</span>
    </div>
    <slot />
    <span v-if="segmentMode === 'qa'" style="opacity: 0.7;">{{ t('knowledgeBase.qaNoChunkRow') }}</span>
  </div>
</template>
