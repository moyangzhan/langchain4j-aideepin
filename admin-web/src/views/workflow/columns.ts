import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'

export interface Data {
  id: string
  uuid: string
  title: string
  remark: string
  isPublic: boolean
  isEnable: boolean
  createTime: string
  updateTime: string
}

export function getColumns(): BasicColumn<Data>[] {
  const { t } = useI18n()
  return [
    {
      title: 'id',
      key: 'id',
      width: 50,
    },
    {
      title: t('columns.title'),
      key: 'title',
      width: 100,
    },
    {
      title: t('columns.remark'),
      key: 'remark',
      width: 150,
    },
    {
      title: t('columns.isPublic'),
      key: 'isPublic',
      width: 100,
      render(row) {
        return row.isPublic ? t('common.yes') : t('common.no')
      },
    },
    {
      title: t('columns.isEnable'),
      key: 'isEnable',
      width: 100,
      render(row) {
        return row.isEnable ? t('common.yes') : t('common.no')
      },
    },
    {
      title: t('columns.userName'),
      key: 'userName',
      width: 120,
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
