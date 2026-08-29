<script setup lang="ts">
import { computed, h } from 'vue'
import type { DataTableColumns } from 'naive-ui'
import { NButton, NDataTable, NIcon, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import { useI18n } from 'vue-i18n'
import { createStatusColumn } from './segment-status'
import ExpandableText from './ExpandableText.vue'

const props = defineProps<{
  segments: KnowledgeBase.Segment[]
  page: number
  pageSize: number
  itemCount: number
  loading: boolean
  maxHeight?: number
}>()

const emit = defineEmits<{
  (e: 'editPair', seg: KnowledgeBase.Segment): void
  (e: 'deleteSegment', uuid: string): void
  (e: 'retry', seg: KnowledgeBase.Segment): void
  (e: 'repair', seg: KnowledgeBase.Segment): void
  (e: 'update:page', page: number): void
}>()

const { t } = useI18n()

function truncated(text: string, len = 60) {
  return text.length > len ? `${text.substring(0, len)}...` : text
}

const columns = computed<DataTableColumns<KnowledgeBase.Segment>>(() => [
  {
    title: '#',
    key: 'position',
    width: 60,
    render: row => row.position + 1,
  },
  {
    // Column header question icon: one answer can carry multiple questions (the cell is the answer's question set)
    title: () => h('span', { class: 'flex items-center gap-1' }, {
      default: () => [
        h('span', t('knowledgeBase.relatedQuestions')),
        h(NTooltip, { trigger: 'hover' }, {
          trigger: () => h(NIcon, { size: 14, style: 'cursor: help; opacity: 0.65;' }, { default: () => h(QuestionCircle16Regular) }),
          default: () => t('knowledgeBase.qaQuestionMultiTip'),
        }),
      ],
    }),
    key: 'questions',
    render: row => h('div', { class: 'flex flex-col gap-1' }, {
      default: () => (row.questions || []).map(q => h('div', { class: 'truncate' }, { default: () => truncated(q.content, 40) })),
    }),
  },
  {
    title: t('knowledgeBase.qaAnswer'),
    key: 'content',
    render: row => h(ExpandableText, { text: row.content, lines: 3 }),
  },
  {
    title: t('knowledgeBase.segmentHitCount'),
    key: 'hitCount',
    width: 90,
  },
  {
    title: t('knowledgeBase.wordCount'),
    key: 'wordCount',
    width: 90,
  },
  createStatusColumn(t, { onRetry: seg => emit('retry', seg), onRepair: seg => emit('repair', seg) }),
  {
    title: t('common.action'),
    key: 'actions',
    width: 100,
    render: row => h('div', { class: 'flex items-center gap-2' }, {
      default: () => [
        h(NButton, { text: true, type: 'primary', size: 'small', onClick: () => emit('editPair', row) }, { default: () => t('common.edit') }),
        h(NButton, { text: true, type: 'error', size: 'small', onClick: () => emit('deleteSegment', row.uuid) }, { default: () => t('common.delete') }),
      ],
    }),
  },
])
</script>

<script lang="ts">
export default { name: 'QaSegmentList' }
</script>

<template>
  <NDataTable
    remote :loading="props.loading" :max-height="props.maxHeight" :columns="columns" :data="props.segments"
    :row-key="(row: KnowledgeBase.Segment) => row.uuid"
    :pagination="{ page: props.page, pageSize: props.pageSize, itemCount: props.itemCount, prefix: () => t('common.total', { n: props.itemCount }) }"
    :single-line="false" :bordered="true" @update:page="(p: number) => emit('update:page', p)"
  />
</template>
