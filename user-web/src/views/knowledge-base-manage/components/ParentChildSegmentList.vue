<script setup lang="ts">
import { computed, h } from 'vue'
import type { DataTableColumns } from 'naive-ui'
import { NButton, NCollapse, NCollapseItem, NDataTable, NEmpty, NIcon, NPagination } from 'naive-ui'
import { CheckmarkCircle12Filled } from '@vicons/fluent'
import { useI18n } from 'vue-i18n'
import { segmentDriftVisible, segmentStatusMeta } from './segment-status'
import ExpandableText from './ExpandableText.vue'

const props = defineProps<{
  segments: KnowledgeBase.Segment[]
  page: number
  pageSize: number
  itemCount: number
  loading: boolean
}>()

const emit = defineEmits<{
  (e: 'editSegment', seg: KnowledgeBase.Segment): void
  (e: 'editChild', child: KnowledgeBase.SegmentChildChunk): void
  (e: 'addChild', parentSegmentId: string): void
  (e: 'deleteChild', uuid: string): void
  (e: 'deleteSegment', uuid: string): void
  (e: 'toggleStatus', seg: KnowledgeBase.Segment): void
  (e: 'rechunk', seg: KnowledgeBase.Segment): void
  (e: 'retry', seg: KnowledgeBase.Segment): void
  (e: 'repair', seg: KnowledgeBase.Segment): void
  (e: 'update:page', page: number): void
}>()

const { t } = useI18n()

function truncated(text: string, len = 80) {
  return text.length > len ? `${text.substring(0, len)}...` : text
}

function metaOf(seg: KnowledgeBase.Segment) {
  return segmentStatusMeta(seg, t)
}

const childColumns = computed<DataTableColumns<KnowledgeBase.SegmentChildChunk>>(() => [
  {
    title: '#',
    key: 'position',
    width: 50,
    render: row => row.position + 1,
  },
  {
    title: t('knowledgeBase.childChunks'),
    key: 'content',
    render: row => h(ExpandableText, { text: row.content, lines: 3 }),
  },
  {
    title: t('knowledgeBase.wordCount'),
    key: 'wordCount',
    width: 90,
  },
  {
    title: t('common.action'),
    key: 'actions',
    width: 100,
    render: row => h('div', { class: 'flex items-center gap-2' }, {
      default: () => [
        h(NButton, { text: true, type: 'primary', size: 'tiny', onClick: () => emit('editChild', row) }, { default: () => t('common.edit') }),
        h(NButton, { text: true, type: 'error', size: 'tiny', onClick: () => emit('deleteChild', row.uuid) }, { default: () => t('common.delete') }),
      ],
    }),
  },
])
</script>

<script lang="ts">
export default { name: 'ParentChildSegmentList' }
</script>

<template>
  <div :style="props.loading ? 'opacity:0.6;pointer-events:none;' : ''">
    <NEmpty v-if="props.segments.length === 0" :description="t('common.noData')" style="margin: 32px 0;" />
    <NCollapse v-else>
      <NCollapseItem v-for="seg in props.segments" :key="seg.uuid" :name="seg.uuid">
        <template #header>
          <div class="pc-header">
            <span class="pc-seq">{{ seg.position + 1 }}</span>
            <span class="pc-preview">{{ truncated(seg.content, 80) }}</span>
            <span v-if="metaOf(seg).iconCheck" class="pc-status" :title="metaOf(seg).title">
              <NIcon :size="14" color="#18a058">
                <CheckmarkCircle12Filled />
              </NIcon>
            </span>
            <span
              v-else class="pc-status" :title="metaOf(seg).title"
              :style="{ fontSize: '12px', color: metaOf(seg).color, opacity: metaOf(seg).muted ? 0.55 : 1, cursor: metaOf(seg).retryable ? 'pointer' : 'default' }"
              @click.stop="metaOf(seg).retryable && emit('retry', seg)"
            >
              {{ metaOf(seg).text }}
            </span>
            <span v-if="segmentDriftVisible(seg)" class="pc-drift" @click.stop="emit('repair', seg)">
              {{ t('knowledgeBase.vectorMissing') }}
            </span>
          </div>
        </template>
        <div class="pc-body">
          <div class="pc-parent">
            {{ seg.content }}
          </div>
          <div class="pc-meta">
            <span>{{ t('knowledgeBase.segmentHitCount') }}: {{ seg.hitCount }}</span>
            <span>{{ t('knowledgeBase.wordCount') }}: {{ seg.wordCount }}</span>
          </div>
          <div class="pc-actions">
            <NButton v-if="seg.isEnabled !== false" text type="primary" size="tiny" @click="emit('addChild', seg.id)">
              + {{ t('knowledgeBase.childChunks') }}
            </NButton>
            <NButton v-if="seg.isEnabled !== false" text type="primary" size="tiny" @click="emit('rechunk', seg)">
              {{ t('knowledgeBase.rechunkChildren') }}
            </NButton>
            <NButton text type="primary" size="tiny" @click="emit('editSegment', seg)">
              {{ t('knowledgeBase.editParent') }}
            </NButton>
            <NButton text type="primary" size="tiny" @click="emit('toggleStatus', seg)">
              {{ seg.isEnabled === false ? t('knowledgeBase.enableParent') : t('knowledgeBase.disableParent') }}
            </NButton>
            <NButton text type="error" size="tiny" @click="emit('deleteSegment', seg.uuid)">
              {{ t('knowledgeBase.deleteParent') }}
            </NButton>
          </div>
          <NDataTable
            size="small" :columns="childColumns" :data="seg.children || []"
            :row-key="(row: KnowledgeBase.SegmentChildChunk) => row.uuid"
            :bordered="true" :single-line="false"
          />
        </div>
      </NCollapseItem>
    </NCollapse>
    <NPagination
      v-if="props.itemCount > props.pageSize" style="margin-top: 12px; display: flex; justify-content: flex-end;"
      :page="props.page" :item-count="props.itemCount" :page-size="props.pageSize"
      :prefix="() => t('common.total', { n: props.itemCount })"
      @update:page="(p: number) => emit('update:page', p)"
    />
  </div>
</template>

<style scoped>
.pc-header {
  display: flex;
  align-items: center;
  gap: 8px;
  min-width: 0;
  width: 100%;
  padding-right: 8px;
}
.pc-seq {
  flex-shrink: 0;
  font-size: 12px;
  opacity: 0.55;
  min-width: 18px;
  text-align: right;
}
.pc-preview {
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.pc-status {
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
}
.pc-drift {
  flex-shrink: 0;
  font-size: 12px;
  color: #d03050;
  cursor: pointer;
}
.pc-body {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 4px 0 8px;
}
.pc-parent {
  white-space: pre-wrap;
  word-break: break-all;
  font-size: 13px;
}
.pc-meta {
  display: flex;
  gap: 16px;
  font-size: 12px;
  opacity: 0.55;
}
.pc-actions {
  display: flex;
  gap: 12px;
}
</style>
