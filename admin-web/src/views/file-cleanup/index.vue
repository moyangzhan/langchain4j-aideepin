<template>
  <n-card :bordered="false" class="proCard">
    <n-alert type="info" :show-icon="true" class="mb-3">
      <div>{{ t('fileCleanup.intro') }}</div>
      <ul class="intro-list">
        <li v-for="point in introPoints" :key="point">{{ point }}</li>
      </ul>
    </n-alert>

    <BasicForm
      :collapsed="false"
      @register="register"
      @submit="handleSubmit"
      @reset="handleReset"
    />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: FileCleanupData) => row.uuid"
      :checked-row-keys="checkedKeys"
      ref="actionRef"
      :scroll-x="1000"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
    >
      <template #tableTitle>
        <n-button
          type="error"
          :disabled="checkedKeys.length === 0"
          :loading="batchDeleting"
          @click="handleBatchDelete"
        >
          <template #icon>
            <n-icon><DeleteOutlined /></n-icon>
          </template>
          {{ t('common.delete') }}{{ checkedKeys.length > 0 ? ` (${checkedKeys.length})` : '' }}
        </n-button>
      </template>
    </BasicTable>
  </n-card>
</template>

<script lang="ts" setup>
  import { h, reactive, ref } from 'vue'
  import { DeleteOutlined } from '@vicons/antd'
  import { BasicTable, TableAction } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import fileCleanupApi from '@/api/fileCleanup'
  import { getColumns, FileCleanupData, previewUrl, isImage, isTextPreviewable } from './columns'
  import { useDialog } from 'naive-ui'
  import { useI18n } from '@/locales'

  const { t } = useI18n()
  const columns = getColumns()
  const dialog = useDialog()
  const actionRef = ref()
  const checkedKeys = ref<Array<string | number>>([])
  const batchDeleting = ref(false)
  const introPoints = [
    t('fileCleanup.introPoint1'),
    t('fileCleanup.introPoint2'),
    t('fileCleanup.introPoint3'),
    t('fileCleanup.introPoint4'),
  ]
  // 当前页 uuid→文件名映射：被拒回报用文件名而非裸 uuid
  // uuid-to-name map of the current page: rejections are reported by file name, not bare uuid
  const nameByUuid = ref<Record<string, string>>({})

  const schemas = reactive<FormSchema[]>([
    {
      field: 'name',
      component: 'NInput',
      label: t('columns.fileName'),
      componentProps: {
        placeholder: t('fileCleanup.searchNamePlaceholder'),
        clearable: true,
      },
    },
    {
      field: 'userName',
      component: 'NInput',
      label: t('columns.ownerName'),
      componentProps: {
        placeholder: t('fileCleanup.searchUserPlaceholder'),
        clearable: true,
      },
    },
  ])

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
  })

  const actionColumn = {
    width: 200,
    title: t('common.action'),
    key: 'action',
    render(record: FileCleanupData) {
      const actions: Array<{ label: string; onClick: () => void }> = []
      if (isImage(record) || isTextPreviewable(record)) {
        actions.push({
          label: t('fileCleanup.open'),
          onClick: () => handleOpen(record),
        })
      }
      actions.push(
        {
          label: t('fileCleanup.download'),
          onClick: () => handleDownload(record),
        },
        {
          label: t('common.delete'),
          onClick: () => handleDelete(record),
        }
      )
      return h(TableAction, { style: 'button', actions })
    },
  }

  const loadDataTable = async (res) => {
    // 每次加载（翻页/刷新）重置勾选：批量删除限本页多选，不跨页累积
    // Reset the selection on every load (page change / refresh): batch delete stays within one page
    checkedKeys.value = []
    const resp = await fileCleanupApi.search({ ...getFieldsValue() }, res)
    const map: Record<string, string> = {}
    ;(resp.data?.records || []).forEach((row) => (map[row.uuid] = row.name))
    nameByUuid.value = map
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

  function onCheckedRow(rowKeys) {
    checkedKeys.value = rowKeys
  }

  // 业务失败（引用复核>0、文件已不存在、存储删除失败）以 200 响应返回，此处归入被拒名单；
  // 网络层错误经 catch 兜底，同样进名单
  // Business rejections (reference found / file gone / storage failure) return HTTP 200 and land in
  // the rejected list; transport errors fall through the catch into the same list
  async function deleteOne(uuid: string): Promise<string | null> {
    try {
      const resp = await fileCleanupApi.deleteOne(uuid)
      return resp.success ? null : resp.message || uuid
    } catch (e: any) {
      return e?.message || uuid
    }
  }

  function handleDelete(record: FileCleanupData) {
    dialog.warning({
      title: t('common.tip'),
      content: `${t('common.deleteConfirmPrefix')} ${record.name} ${t(
        'common.deleteConfirmSuffix'
      )}`,
      positiveText: t('common.positiveText'),
      negativeText: t('common.negativeText'),
      onPositiveClick: async () => {
        const rejected = await deleteOne(record.uuid)
        if (rejected) {
          window['$message'].warning(`${record.name}: ${rejected}`)
        } else {
          window['$message'].success(t('common.deleteSuccess'))
        }
        reloadTable()
      },
    })
  }

  function handleOpen(record: FileCleanupData) {
    const url = previewUrl(record)
    if (isImage(record)) {
      window.open(url, '_blank')
      return
    }
    // txt/csv 拉取文本弹窗预览；拉取失败（如 OSS 跨域）退化为新标签页打开
    // txt/csv are fetched and previewed in a dialog; on fetch failure (e.g. cross-origin OSS)
    // fall back to opening a new tab
    fetch(url)
      .then((resp) => resp.text())
      .then((text) => {
        dialog.info({
          title: `${t('fileCleanup.previewTitle')} - ${record.name}`,
          content: () =>
            h(
              'pre',
              {
                style:
                  'max-height: 400px; overflow: auto; text-align: left; white-space: pre-wrap; word-break: break-all; margin: 0',
              },
              text.slice(0, 100000)
            ),
          positiveText: t('common.positiveText'),
        })
      })
      .catch(() => window.open(url, '_blank'))
  }

  function handleDownload(record: FileCleanupData) {
    // 同源链接 download 属性强制保存；OSS 跨域外链浏览器忽略 download 属性，退化为新标签页打开
    // Same-origin links honor the download attribute; browsers ignore it for cross-origin OSS links
    // and open a tab instead
    const link = document.createElement('a')
    link.href = previewUrl(record)
    link.download = record.name || `${record.uuid}.${record.ext}`
    link.click()
  }

  function reportRejected(rejected: string[], deletedCount: number) {
    if (rejected.length === 0) {
      window['$message'].success(t('common.deleteSuccess'))
      return
    }
    dialog.info({
      title: t('fileCleanup.rejectedTitle'),
      content: () =>
        h(
          'ul',
          {
            style:
              'max-height: 300px; overflow: auto; text-align: left; padding-left: 20px; margin: 0',
          },
          rejected.map((item) => h('li', item))
        ),
      positiveText: t('common.positiveText'),
    })
    if (deletedCount > 0) {
      window['$message'].success(`${t('common.deleteSuccess')} (${deletedCount})`)
    }
  }

  function handleBatchDelete() {
    dialog.warning({
      title: t('common.tip'),
      content: `${t('common.deleteConfirmPrefix')} ${checkedKeys.value.length} ${t(
        'fileCleanup.selectedFiles'
      )} ${t('common.deleteConfirmSuffix')}`,
      positiveText: t('common.positiveText'),
      negativeText: t('common.negativeText'),
      onPositiveClick: async () => {
        batchDeleting.value = true
        const rejected: string[] = []
        let deletedCount = 0
        try {
          // 按序逐条删除，每条独立复核；被拒的跳过并继续，最后回报名单
          // Sequential deletes, each independently re-verified; rejected ones are skipped and reported
          for (const uuid of checkedKeys.value) {
            const reason = await deleteOne(uuid)
            if (reason) {
              rejected.push(`${nameByUuid.value[uuid] || uuid}: ${reason}`)
            } else {
              deletedCount++
            }
          }
        } finally {
          batchDeleting.value = false
        }
        checkedKeys.value = []
        reportRejected(rejected, deletedCount)
        reloadTable()
      },
    })
  }
</script>

<style lang="less" scoped>
  .intro-list {
    margin: 4px 0 0;
    padding-left: 20px;
    list-style: disc;
  }
</style>
