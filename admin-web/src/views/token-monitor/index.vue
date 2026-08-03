<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: TokenMonitorData) => row.id"
      ref="actionRef"
      :scroll-x="1200"
    />
  </n-card>
</template>

<script lang="ts" setup>
  import { ref, reactive, onMounted } from 'vue'
  import { BasicTable } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import tokenMonitorApi from '@/api/tokenMonitor'
  import modelPlatformApi from '@/api/modelPlatform'
  import { getColumns, getSourceTypeOptions, TokenMonitorData } from './columns'
  const columns = getColumns()
  import { t } from '@/locales'

  // 平台下拉选项，由「平台配置」列表加载 | platform options, loaded from the platform config list
  const platformOptions = reactive<{ label: string; value: string }[]>([])

  const schemas = reactive<FormSchema[]>([
    {
      field: 'userName',
      component: 'NInput',
      label: t('tokenMonitor.userName'),
      componentProps: {
        placeholder: t('tokenMonitor.userNamePlaceholder'),
      },
    },
    {
      field: 'modelPlatform',
      component: 'NSelect',
      label: t('columns.modelPlatform'),
      componentProps: {
        placeholder: t('tokenMonitor.modelPlatformPlaceholder'),
        options: platformOptions,
        clearable: true,
      },
    },
    {
      field: 'modelName',
      component: 'NInput',
      label: t('columns.modelName'),
      componentProps: {
        placeholder: t('tokenMonitor.modelNamePlaceholder'),
      },
    },
    {
      field: 'sourceType',
      component: 'NSelect',
      label: t('columns.sourceType'),
      componentProps: {
        placeholder: t('tokenMonitor.sourceTypePlaceholder'),
        options: getSourceTypeOptions(),
        clearable: true,
      },
    },
    {
      field: 'requestTime',
      component: 'NDatePicker',
      label: t('columns.requestTime'),
      componentProps: {
        type: 'datetimerange',
        clearable: true,
      },
    },
  ])

  const actionRef = ref()

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
    collapsed: false,
  })

  onMounted(async () => {
    try {
      const resp = await modelPlatformApi.search({}, { current: 1, size: 100 })
      const list = resp.data?.records || []
      const options = list.map((p: any) => ({ label: p.title || p.name, value: p.name }))
      platformOptions.push(...options)
    } catch (e) {
      // 加载平台列表失败时忽略，下拉保持为空 | ignore on failure, dropdown stays empty
    }
  })

  const loadDataTable = async (res) => {
    const resp = await tokenMonitorApi.search({ ...getFieldsValue() }, res)
    return resp.data
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    reloadTable()
  }
</script>

<style lang="less" scoped></style>
