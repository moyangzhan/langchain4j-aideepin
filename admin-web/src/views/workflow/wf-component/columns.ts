import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'

export interface ComponentData {
  id: string
  uuid: string
  name: string
  title: string
  remark: string
  createTime: string
  updateTime: string
  isEnable: boolean
}

export function getColumns(): BasicColumn<ComponentData>[] {
  const { t } = useI18n()
  return [
    {
      title: 'id',
      key: 'id',
      width: 50,
    },
    {
      title: t('columns.name'),
      key: 'name',
      width: 130,
    },
    {
      title: t('columns.title'),
      key: 'title',
      width: 100,
    },
    {
      title: t('columns.description'),
      key: 'remark',
      width: 260,
    },
    {
      title: t('columns.displayOrder'),
      key: 'displayOrder',
      width: 80,
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
    {
      title: t('columns.isEnable'),
      key: 'isEnable',
      width: 100,
      render(row) {
        return row.isEnable ? t('common.yes') : t('common.no')
      },
    },
  ]
}
