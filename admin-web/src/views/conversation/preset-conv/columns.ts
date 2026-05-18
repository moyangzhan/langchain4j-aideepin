import { NTag } from 'naive-ui'
import { h } from 'vue'
import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { CharacterPreset } from '/#/conversation'

const typeLabels: Record<string, string> = {
  technology: 'character.presetTypeTechnology',
  creative: 'character.presetTypeCreative',
  education: 'character.presetTypeEducation',
  business: 'character.presetTypeBusiness',
  professional: 'character.presetTypeProfessional',
  design: 'character.presetTypeDesign',
  marketing: 'character.presetTypeMarketing',
  service: 'character.presetTypeService',
  administration: 'character.presetTypeAdministration',
  utility: 'character.presetTypeUtility',
}

export function getColumns(): BasicColumn<CharacterPreset>[] {
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
      title: t('character.presetType'),
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
