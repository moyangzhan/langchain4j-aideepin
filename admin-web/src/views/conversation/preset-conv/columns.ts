import { NTag } from 'naive-ui'
import { h } from 'vue'
import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { ConversationPreset } from '/#/conversation'

const typeLabels: Record<string, string> = {
  technology: 'conversation.presetTypeTechnology',
  creative: 'conversation.presetTypeCreative',
  education: 'conversation.presetTypeEducation',
  business: 'conversation.presetTypeBusiness',
  professional: 'conversation.presetTypeProfessional',
  design: 'conversation.presetTypeDesign',
  marketing: 'conversation.presetTypeMarketing',
  service: 'conversation.presetTypeService',
  administration: 'conversation.presetTypeAdministration',
  utility: 'conversation.presetTypeUtility',
}

export function getColumns(): BasicColumn<ConversationPreset>[] {
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
      width: 100,
    },
    {
      title: t('conversation.presetType'),
      key: 'type',
      width: 120,
      render: (row) => {
        const label = typeLabels[row.type]
        return h(NTag, { size: 'small', type: 'info' }, () => label ? t(label) : row.type)
      },
    },
    {
      title: t('columns.description'),
      key: 'remark',
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
