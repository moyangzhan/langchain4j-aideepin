import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { Conversation } from '/#/conversation'

export function getColumns(): BasicColumn<Conversation>[] {
  const { t } = useI18n()
  return [
    {
      title: 'id',
      key: 'id',
      width: 50,
    },
    {
      title: 'uuid',
      key: 'uuid',
      width: 120,
    },
    {
      title: t('columns.title'),
      key: 'title',
      width: 150,
    },
    {
      title: t('columns.aiSystemMessage'),
      key: 'aiSystemMessage',
      width: 200,
    },
    {
      title: t('columns.tokens'),
      key: 'tokens',
      width: 150,
    },
    {
      title: t('columns.understandContextEnable'),
      key: 'understandContextEnable',
      width: 100,
      render(row) {
        return row.understandContextEnable ? t('common.yes') : t('common.no')
      },
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
