import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'

export interface AiPlatformData {
  id: string
  name: string
  title: string
  baseUrl: string
  apiKey: string
  secretKey: string
  isProxyEnable: boolean
  isOpenaiApiCompatible: boolean
  createTime: string
  updateTime: string
}

export function getColumns(): BasicColumn<AiPlatformData>[] {
  const { t } = useI18n()
  return [
    {
      title: t('columns.name'),
      key: 'name',
      width: 120,
    },
    {
      title: t('columns.title'),
      key: 'title',
    },
    {
      title: t('columns.baseUrl'),
      key: 'baseUrl',
    },
    {
      title: t('columns.apiKey'),
      key: 'apiKey',
      width: 100,
      render(row) {
        return row.apiKey ? t('constants.configured') : t('constants.notConfigured')
      },
    },
    {
      title: t('columns.secretKey'),
      key: 'secretKey',
      width: 100,
      render(row) {
        return row.secretKey ? t('constants.configured') : '-'
      },
    },
    {
      title: t('columns.isProxyEnable'),
      key: 'isProxyEnable',
      render(row) {
        return row.isProxyEnable ? t('common.enable') : t('common.disable')
      },
    },
    {
      title: t('columns.isOpenaiApiCompatible'),
      key: 'isOpenaiApiCompatible',
      render(row) {
        return row.isOpenaiApiCompatible ? t('common.yes') : t('common.no')
      },
    },
  ]
}
