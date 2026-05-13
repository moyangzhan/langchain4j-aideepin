import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { McpInfo } from '/#/mcp'

export function getColumns(): BasicColumn<McpInfo>[] {
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
      title: t('mcp.transportType'),
      key: 'transportType',
      width: 100,
      render(row) {
        const types: Record<string, string> = {
          sse: t('constants.sse'),
          stdio: t('constants.stdio'),
        }
        return types[row.transportType] || row.transportType
      },
    },
    {
      title: t('mcp.installType'),
      key: 'installType',
      width: 100,
      render(row) {
        const types: Record<string, string> = {
          docker: t('constants.docker'),
          local: t('constants.local'),
          remote: t('constants.remote'),
          wasm: t('constants.wasm'),
        }
        return types[row.installType] || row.installType
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
