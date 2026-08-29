import { h } from 'vue'
import type { DataTableColumn } from 'naive-ui'
import { NIcon } from 'naive-ui'
import { CheckmarkCircle12Filled } from '@vicons/fluent'

export interface SegmentStatusMeta {
  kind: 'disabled' | 'doing' | 'fail' | 'done' | 'pending'
  text: string
  color: string
  muted: boolean
  iconCheck: boolean
  title: string
  retryable: boolean
}

/**
 * View model of the segment vectorization status badge, shared by the status column
 * of the segment table and the parent-child collapse headers.
 */
export function segmentStatusMeta(seg: KnowledgeBase.Segment, t: (key: string) => string): SegmentStatusMeta {
  if (seg.isEnabled === false)
    return { kind: 'disabled', text: t('knowledgeBase.statusDisabled'), color: '', muted: true, iconCheck: false, title: '', retryable: false }

  switch (seg.embeddingStatus) {
    case 'DOING':
      return { kind: 'doing', text: t('knowledgeBase.statusProcessing'), color: '#f0a020', muted: false, iconCheck: false, title: '', retryable: false }
    case 'FAIL':
      return { kind: 'fail', text: t('knowledgeBase.statusFailed'), color: '#d03050', muted: false, iconCheck: false, title: seg.failReason || '', retryable: true }
    case 'DONE':
      return { kind: 'done', text: t('knowledgeBase.statusVectorized'), color: '#18a058', muted: false, iconCheck: true, title: t('knowledgeBase.statusVectorized'), retryable: false }
    default:
      return { kind: 'pending', text: t('knowledgeBase.statusPending'), color: '', muted: true, iconCheck: false, title: '', retryable: false }
  }
}

export function isSegmentRebuilding(seg: KnowledgeBase.Segment) {
  return seg.embeddingStatus === 'DOING' || seg.graphicalStatus === 'DOING'
}

export function segmentDriftVisible(seg: KnowledgeBase.Segment) {
  return !!seg.vectorMissing && !isSegmentRebuilding(seg)
}

/**
 * Pure vectorization status column, shared by the text segment table and QaSegmentList.
 */
export function createStatusColumn(
  t: (key: string) => string,
  handlers: { onRetry: (seg: KnowledgeBase.Segment) => void; onRepair: (seg: KnowledgeBase.Segment) => void },
): DataTableColumn<KnowledgeBase.Segment> {
  return {
    title: t('knowledgeBase.vectorize'),
    key: 'embeddingStatus',
    width: 120,
    render: (row) => {
      const meta = segmentStatusMeta(row, t)
      const elements: any[] = []
      if (meta.iconCheck) {
        // Vectorized is the steady state: a green check reads faster than text (title keeps the label)
        elements.push(h(NIcon, { size: 14, color: '#18a058', title: meta.title }, { default: () => h(CheckmarkCircle12Filled) }))
      } else {
        elements.push(h('span', {
          style: `font-size:12px;color:${meta.color};${meta.muted ? 'opacity:0.55;' : ''}${meta.retryable ? 'cursor:pointer;' : ''}`,
          title: meta.title || undefined,
          onClick: meta.retryable ? () => handlers.onRetry(row) : undefined,
        }, { default: () => meta.text }))
      }
      // Drift indicator: status says vectorized but the store lacks the vector
      if (segmentDriftVisible(row))
        elements.push(h('span', { style: 'font-size:12px;color:#d03050;margin-left:6px;cursor:pointer;', onClick: () => handlers.onRepair(row) }, { default: () => t('knowledgeBase.vectorMissing') }))
      return h('div', { class: 'flex items-center' }, { default: () => elements })
    },
  }
}
