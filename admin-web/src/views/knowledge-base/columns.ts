import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { wrapTableTitle } from '@/utils'

export interface KbInfoData {
  id: string
  uuid: string
  title: string
  remark: string
  ownerUuid: string
  ownerName: string
  isPublic: boolean
  starCount: number
  itemCount: number
  embeddingCount: number
  createTime: string
  updateTime: string
}

export function getColumns(): BasicColumn<KbInfoData>[] {
  const { t } = useI18n()
  return [
    {
      title: 'id',
      key: 'id',
      width: 50,
    },
    {
      title: t('columns.name'),
      key: 'title',
      width: 150,
    },
    {
      title: t('columns.ownerName'),
      key: 'ownerName',
      width: 150,
    },
    {
      title: t('columns.itemCount'),
      key: 'itemCount',
      width: 80,
    },
    {
      title: t('columns.embeddingCount'),
      key: 'embeddingCount',
      width: 80,
    },
    {
      title: t('columns.starCount'),
      key: 'starCount',
      width: 80,
    },
    {
      title: t('columns.isPublic'),
      key: 'isPublic',
      width: 80,
      render(row) {
        return row.isPublic ? t('common.yes') : t('common.no')
      },
    },
    {
      key: 'ingestMaxOverlap',
      width: 100,
      title(column) {
        return wrapTableTitle(t('columns.ingestMaxOverlap'))
      },
    },
    {
      key: 'ingestModelName',
      width: 160,
      title(column) {
        return wrapTableTitle(t('columns.ingestModelName'))
      },
    },
    {
      key: 'retrieveMaxResults',
      width: 100,
      title(column) {
        return wrapTableTitle(t('columns.retrieveMaxResults'))
      },
    },
    {
      key: 'retrieveMinScore',
      width: 100,
      title(column) {
        return wrapTableTitle(t('columns.retrieveMinScore'))
      },
    },
    {
      key: 'queryLlmTemperature',
      width: 120,
      title(column) {
        return wrapTableTitle(t('columns.queryLlmTemperature'))
      },
    },
    {
      title: t('columns.querySystemMessage'),
      key: 'querySystemMessage',
      width: 150,
    },
    {
      title: t('columns.createTime'),
      key: 'createTime',
      width: 180,
    },
    {
      title: t('columns.updateTime'),
      key: 'updateTime',
      width: 180,
    },
  ]
}
