import type { DataTableColumns } from 'naive-ui'
import { h } from 'vue'
import type { VNode } from 'vue'
import { NButton, NEllipsis } from 'naive-ui'
import { t } from '@/locales'

export const createColumns = (showEmbeddingListFn: Function, showGraphFn: Function, showFileContentFn: Function, changeItemShowModalFn: Function, deleteKbItemFn: Function): DataTableColumns<KnowledgeBase.Item> => {
  return [
    {
      type: 'selection',
    },
    {
      title: t('knowledgeBase.itemTitle'),
      key: 'title',
      width: 200,
    },
    {
      title: t('knowledgeBase.brief'),
      key: 'brief',
      render(row) {
        return row.brief.substring(0, 50)
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
          renderElements.push(createShowListButton(showEmbeddingListFn, row))
          renderElements.push(createText(t('knowledgeBase.statusProcessing')))
          renderElements.push(createText(row.embeddingStatusChangeTime))
        } else if (row.embeddingStatus === 'DONE') {
          renderElements.push(createShowListButton(showEmbeddingListFn, row))
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
          renderElements.push(createShowListButton(showGraphFn, row))
          renderElements.push(createText(t('knowledgeBase.statusProcessing')))
          renderElements.push(createText(row.graphicalStatusChangeTime))
        } else if (row.graphicalStatus === 'DONE') {
          renderElements.push(createShowListButton(showGraphFn, row))
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
            onClick: () => showFileContentFn(row),
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
      width: 100,
      align: 'center',
      render(row) {
        return h('div', { class: 'flex items-center flex-col gap-2' }, {
          default: () => [
            h(
              NButton,
              {
                tertiary: true,
                size: 'small',
                type: 'info',
                onClick: () => changeItemShowModalFn(row),
              },
              { default: () => t('common.edit') },
            ),
            h(
              NButton,
              {
                tertiary: true,
                size: 'small',
                type: 'error',
                onClick: () => deleteKbItemFn(row),
              },
              { default: () => t('common.delete') },
            ),
          ],
        })
      },
    },
  ]
}

function createShowListButton(showListFn: Function, row: KnowledgeBase.Item) {
  return h(
    NButton,
    {
      text: true,
      size: 'small',
      type: 'info',
      onClick: () => showListFn(row),
    },
    { default: () => t('common.view') },
  )
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
