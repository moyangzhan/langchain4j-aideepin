import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { AiModelData } from '/#/aiModel'

export function getColumns(): BasicColumn<AiModelData>[] {
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
      width: 180,
    },
    {
      title: t('columns.title'),
      key: 'title',
      width: 180,
    },
    {
      title: t('columns.type'),
      key: 'type',
      width: 80,
    },
    {
      title: t('columns.platform'),
      key: 'platform',
      width: 120,
    },
    {
      title: t('columns.isEnable'),
      key: 'isEnable',
      width: 80,
      render(row) {
        return row.isEnable ? t('common.enable') : t('common.disable')
      },
    },
    {
      title: t('columns.isFree'),
      key: 'isFree',
      width: 80,
      render(row) {
        return row.isFree ? t('common.yes') : t('common.no')
      },
    },
    {
      title: t('columns.contextWindow'),
      key: 'contextWindow',
      width: 100,
    },
    {
      title: t('columns.maxInputTokens'),
      key: 'maxInputTokens',
      width: 140,
    },
    {
      title: t('columns.maxOutputTokens'),
      key: 'maxOutputTokens',
      width: 140,
    },
    {
      title: t('columns.setting'),
      key: 'setting',
    },
    {
      title: t('columns.createTime'),
      key: 'createTime',
      width: 150,
    },
    {
      title: t('columns.updateTime'),
      key: 'updateTime',
      width: 150,
    },
  ]
}
