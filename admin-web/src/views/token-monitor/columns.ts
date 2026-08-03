import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'

export interface TokenMonitorData {
  id: number
  uuid: string
  sourceType: number
  sourceId: number
  userId: number
  userName: string
  modelPlatform: string
  modelName: string
  inputTokens: number
  outputTokens: number
  duration: number
  requestTime: string
}

/**
 * 来源类型下拉选项，供搜索表单与列表渲染复用 | Source type options, shared by the search form and the list render
 */
export function getSourceTypeOptions() {
  const { t } = useI18n()
  return [
    { label: t('columns.sourceUnknown'), value: 0 },
    { label: t('columns.sourceCharacterChat'), value: 1 },
    { label: t('columns.sourceKnowledgeBaseQa'), value: 2 },
    { label: t('columns.sourceKnowledgeBaseIngest'), value: 3 },
    { label: t('columns.sourceWorkflowNode'), value: 4 },
    { label: t('columns.sourceAgent'), value: 5 },
    { label: t('columns.sourceLongTermMemoryExtraction'), value: 6 },
    { label: t('columns.sourceLongTermMemoryAnalysis'), value: 7 },
  ]
}

export function getColumns(): BasicColumn<TokenMonitorData>[] {
  const { t } = useI18n()
  const sourceTypeMap = new Map(getSourceTypeOptions().map((o) => [o.value, o.label]))
  return [
    {
      title: 'id',
      key: 'id',
      width: 50,
    },
    {
      title: t('tokenMonitor.userName'),
      key: 'userName',
      width: 100,
    },
    {
      title: t('columns.sourceType'),
      key: 'sourceType',
      width: 150,
      render(row) {
        return sourceTypeMap.get(row.sourceType) ?? t('common.unknown')
      },
    },
    {
      title: t('columns.modelPlatform'),
      key: 'modelPlatform',
      width: 120,
    },
    {
      title: t('columns.modelName'),
      key: 'modelName',
      width: 160,
    },
    {
      title: t('columns.inputTokens'),
      key: 'inputTokens',
      width: 110,
    },
    {
      title: t('columns.outputTokens'),
      key: 'outputTokens',
      width: 110,
    },
    {
      title: t('columns.duration'),
      key: 'duration',
      width: 110,
      render(row) {
        return row.duration != null ? `${row.duration} ms` : ''
      },
    },
    {
      title: t('columns.requestTime'),
      key: 'requestTime',
      width: 180,
    },
  ]
}
