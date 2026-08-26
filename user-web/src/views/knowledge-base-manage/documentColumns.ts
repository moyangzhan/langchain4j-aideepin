import type { DataTableColumns } from 'naive-ui'
import { h } from 'vue'
import type { VNode } from 'vue'
import { NButton, NEllipsis, NSwitch, NTag, NTooltip } from 'naive-ui'
import { RouterLink } from 'vue-router'
import { t } from '@/locales'

interface DocumentColumnCallbacks {
  viewSegments: (row: KnowledgeBase.Document) => void
  showGraph: (row: KnowledgeBase.Document) => void
  showFileContent: (row: KnowledgeBase.Document) => void
  editItem: (row: KnowledgeBase.Document) => void
  deleteKbItem: (row: KnowledgeBase.Document) => void
  toggleStatus: (row: KnowledgeBase.Document, isEnabled: boolean) => void
}

export const createColumns = (callbacks: DocumentColumnCallbacks): DataTableColumns<KnowledgeBase.Document> => {
  return [
    {
      type: 'selection',
    },
    {
      title: t('knowledgeBase.itemTitle'),
      key: 'title',
      width: 200,
      render(row) {
        return h(
          RouterLink,
          {
            class: 'hljs-link',
            to: {
              name: 'DocumentDetail',
              params: {
                kbUuid: row.kbUuid,
                docUuid: row.uuid,
              },
            },
          },
          { default: () => row.title },
        )
      },
    },
    {
      title: t('knowledgeBase.segmentMode'),
      key: 'segmentMode',
      width: 120,
      render(row) {
        return h(
          NTag,
          {
            size: 'small',
            bordered: false,
            type: segmentModeTagType(row.segmentMode),
          },
          { default: () => segmentModeLabel(row.segmentMode) },
        )
      },
    },
    {
      title: t('knowledgeBase.vectorize'),
      key: 'embeddingStatus',
      width: 150,
      render(row) {
        const renderElements: VNode[] = []
        if (row.embeddingStatus === 'NONE') {
          renderElements.push(createText(t('knowledgeBase.statusPending')))
        } else if (row.embeddingStatus === 'DOING') {
          renderElements.push(createText(t('knowledgeBase.statusProcessing')))
          renderElements.push(createText(row.embeddingStatusChangeTime))
        } else if (row.embeddingStatus === 'DONE') {
          renderElements.push(createText(t('knowledgeBase.statusVectorized')))
          renderElements.push(createText(row.embeddingStatusChangeTime))
        } else if (row.embeddingStatus === 'FAIL') {
          renderElements.push(createText(t('knowledgeBase.statusFailed')))
          renderElements.push(createText(row.embeddingStatusChangeTime))
        }
        return h('div', { class: 'flex flex-col' }, {
          default: () => renderElements,
        })
      },
    },
    {
      title: t('knowledgeBase.graphLabel'),
      key: 'graphicalStatus',
      width: 150,
      render(row) {
        const renderElements: VNode[] = []
        if (row.graphicalStatus === 'NONE') {
          renderElements.push(createText(t('knowledgeBase.statusPending')))
        } else if (row.graphicalStatus === 'DOING') {
          renderElements.push(createText(t('knowledgeBase.statusProcessing')))
          renderElements.push(createText(row.graphicalStatusChangeTime))
        } else if (row.graphicalStatus === 'DONE') {
          renderElements.push(createText(t('knowledgeBase.statusGraphitized')))
          renderElements.push(createText(row.graphicalStatusChangeTime))
        } else if (row.graphicalStatus === 'FAIL') {
          renderElements.push(createText(t('knowledgeBase.statusFailed')))
          renderElements.push(createText(row.graphicalStatusChangeTime))
        }
        return h('div', { class: 'flex flex-col' }, {
          default: () => renderElements,
        })
      },
    },
    {
      title: t('knowledgeBase.attachment'),
      key: 'sourceFileName',
      width: 150,
      render(row) {
        const soureFile = !!row.sourceFileUuid
        if (soureFile) {
          return h('div', {
            class: 'flex flex-col',
            onClick: () => callbacks.showFileContent(row),
          },
          {
            default: () => [h(
              NEllipsis,
              {
                lineClamp: 3,
                style: 'color:#2080f0;cursor:pointer',
              },
              { default: () => row.sourceFileName || row.title },
            ),
            ],
          })
        } else {
          return t('common.none')
        }
      },
    },
    {
      title: t('knowledgeBase.wordCount'),
      key: 'wordCount',
      width: 100,
    },
    {
      title: t('knowledgeBase.embeddingHitCount'),
      key: 'embeddingHitCount',
      width: 100,
    },
    {
      title: t('knowledgeBase.graphHitCount'),
      key: 'graphHitCount',
      width: 100,
    },
    {
      title: t('knowledgeBase.enabled'),
      key: 'isEnabled',
      width: 100,
      align: 'center',
      render(row) {
        return h(NSwitch, {
          value: row.isEnabled,
          size: 'small',
          onUpdateValue: (val: boolean) => callbacks.toggleStatus(row, val),
        })
      },
    },
    {
      title: t('knowledgeBase.createTime'),
      key: 'createTime',
      width: 180,
    },
    {
      title: t('knowledgeBase.updateTime'),
      key: 'updateTime',
      width: 180,
    },
    {
      title: t('common.action'),
      key: 'actions',
      width: 120,
      align: 'center',
      render(row) {
        return h('div', { class: 'flex items-center flex-col gap-2' }, {
          default: () => [
            h('div', { class: 'flex gap-1' }, [
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'small',
                  type: 'info',
                  onClick: () => callbacks.viewSegments(row),
                },
                { default: () => t('knowledgeBase.viewSegments') },
              ),
              createGraphActionButton(callbacks.showGraph, row),
            ]),
            h('div', { class: 'flex gap-1' }, [
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'small',
                  type: 'info',
                  onClick: () => callbacks.editItem(row),
                },
                { default: () => t('common.edit') },
              ),
              h(
                NButton,
                {
                  tertiary: true,
                  size: 'small',
                  type: 'error',
                  onClick: () => callbacks.deleteKbItem(row),
                },
                { default: () => t('common.delete') },
              ),
            ]),
          ],
        })
      },
    },
  ]
}

// Legacy rows may lack segmentMode; unset falls back to text
function segmentModeLabel(segmentMode?: string) {
  if (segmentMode === 'qa')
    return t('knowledgeBase.segmentModeQa')
  if (segmentMode === 'parent_child')
    return t('knowledgeBase.segmentModeParentChild')
  return t('knowledgeBase.segmentModeText')
}

function segmentModeTagType(segmentMode?: string) {
  if (segmentMode === 'qa')
    return 'success'
  if (segmentMode === 'parent_child')
    return 'info'
  return 'default'
}

// Graph button: disabled with a tooltip when not graphitized (NONE); DOING shows the partial
// graph, FAIL may still hold partial data
function createGraphActionButton(showGraphFn: (row: KnowledgeBase.Document) => void, row: KnowledgeBase.Document) {
  const notGraphitized = row.graphicalStatus === 'NONE'
  const button = h(
    NButton,
    {
      tertiary: true,
      size: 'small',
      type: 'info',
      disabled: notGraphitized,
      onClick: () => showGraphFn(row),
    },
    { default: () => t('knowledgeBase.viewGraph') },
  )
  if (!notGraphitized)
    return button
  // Native disabled buttons swallow mouse events, so the tooltip wraps the button
  return h(NTooltip, { trigger: 'hover' }, {
    trigger: () => h('span', { class: 'inline-flex' }, [button]),
    default: () => t('knowledgeBase.notGraphitized'),
  })
}

function createText(txt: string) {
  return h(
    'div',
    {
      style: 'font-size: 10px',
      class: 'mt-1',
    },
    { default: () => txt },
  )
}
