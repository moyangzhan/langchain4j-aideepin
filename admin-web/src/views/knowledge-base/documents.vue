<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: KbDocData) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      :scroll-x="1160"
    />

    <n-modal
      v-model:show="showEditModal"
      :show-icon="false"
      preset="dialog"
      :title="t('common.edit')"
    >
      <n-form
        :model="editFormParams"
        :rules="editRules"
        ref="formRef"
        label-placement="left"
        :label-width="80"
        class="py-4"
      >
        <n-form-item :label="t('columns.title')" path="title">
          <n-input v-model:value="editFormParams.title" maxlength="250" show-count />
        </n-form-item>
        <n-form-item :label="t('common.description')" path="brief">
          <n-input
            v-model:value="editFormParams.brief"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 5 }"
            maxlength="250"
            show-count
          />
        </n-form-item>
      </n-form>
      <template #action>
        <n-space>
          <n-button @click="() => (showEditModal = false)">{{ t('common.cancel') }}</n-button>
          <n-button type="info" :loading="formBtnLoading" @click="confirmEditForm">{{
            t('common.confirm')
          }}</n-button>
        </n-space>
      </template>
    </n-modal>
  </n-card>
</template>

<script lang="ts" setup>
  import { h, onMounted, reactive, ref } from 'vue'
  import { useRoute } from 'vue-router'
  import { BasicTable, TableAction } from '@/components/Table'
  import type { BasicColumn } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import documentApi from '@/api/document'
  import kbApi from '@/api/knowledgeBase'
  import { useDialog } from 'naive-ui'
  import type { FormRules } from 'naive-ui'
  import { t } from '@/locales'

  interface KbDocData {
    id: string
    uuid: string
    title: string
    kbTitle: string
    brief: string
    segmentMode: string
    embeddingStatus: string
    wordCount: number
    updateTime: string
  }

  interface SelectOpt {
    label: string
    value: string
  }

  const route = useRoute()
  const dialog = useDialog()

  const initialKbUuid = (route.query.kbUuid as string) || ''
  const initialKbTitle = (route.query.title as string) || ''

  const actionRef = ref()
  const formRef: any = ref(null)

  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    uuid: '',
    title: '',
    brief: '',
  })

  const editRules: FormRules = {
    title: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('common.fillCompleteInfo'),
    },
  }

  // knowledge base titles seen so far, so the selected option survives option list replacement
  const kbLabelByUuid = new Map<string, string>()
  const kbSelectProps = reactive({
    placeholder: t('knowledgeBase.kbSelectPlaceholder'),
    options: [] as SelectOpt[],
    filterable: true,
    remote: true,
    loading: false,
    onSearch: (query: string) => searchKbOptions(query),
  })

  const schemas: FormSchema[] = [
    {
      field: 'kbUuid',
      component: 'NSelect',
      label: t('knowledgeBase.kbSelectLabel'),
      defaultValue: initialKbUuid || undefined,
      componentProps: kbSelectProps,
    },
    {
      field: 'keyword',
      component: 'NInput',
      label: t('common.title'),
      componentProps: {
        placeholder: t('knowledgeBase.documentKeywordPlaceholder'),
      },
    },
  ]

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
  })

  const segmentModeLabels: Record<string, string> = {
    text: t('columns.segmentModeText'),
    qa: t('columns.segmentModeQa'),
    parent_child: t('columns.segmentModeParentChild'),
  }

  const embeddingStatusLabels: Record<string, string> = {
    NONE: t('columns.embeddingStatusNone'),
    DOING: t('columns.embeddingStatusDoing'),
    DONE: t('columns.embeddingStatusDone'),
    FAIL: t('columns.embeddingStatusFail'),
  }

  const columns: BasicColumn<KbDocData>[] = [
    {
      title: t('columns.title'),
      key: 'title',
      width: 220,
      ellipsis: true,
    },
    {
      title: t('columns.kbTitle'),
      key: 'kbTitle',
      width: 160,
      ellipsis: true,
    },
    {
      title: t('columns.segmentMode'),
      key: 'segmentMode',
      width: 110,
      render(row) {
        return segmentModeLabels[row.segmentMode] || row.segmentMode
      },
    },
    {
      title: t('columns.embeddingStatus'),
      key: 'embeddingStatus',
      width: 120,
      render(row) {
        return embeddingStatusLabels[row.embeddingStatus] || row.embeddingStatus
      },
    },
    {
      title: t('columns.wordCount'),
      key: 'wordCount',
      width: 90,
    },
    {
      title: t('columns.updateTime'),
      key: 'updateTime',
      width: 170,
    },
  ]

  const actionColumn = reactive({
    width: 150,
    title: t('common.action'),
    key: 'action',
    fixed: 'right',
    render(record) {
      return h(TableAction as any, {
        style: 'button',
        actions: [
          {
            label: t('common.edit'),
            onClick: handleEdit.bind(null, record),
          },
        ],
        dropDownActions: [
          {
            label: t('common.delete'),
            key: 'delete',
          },
        ],
        select: (key) => {
          if (key === 'delete') {
            dialog.warning({
              title: t('common.tip'),
              content: `${t('common.deleteConfirmPrefix')} ${record.title} ${t(
                'common.deleteConfirmSuffix'
              )}`,
              positiveText: t('common.positiveText'),
              negativeText: t('common.negativeText'),
              onPositiveClick: () => {
                handleDelete(record)
              },
              onNegativeClick: () => {
                // 取消删除
              },
            })
          }
        },
      })
    },
  })

  const loadDataTable = async (res) => {
    const { kbUuid, keyword } = getFieldsValue()
    const resp = await documentApi.search(kbUuid || '', keyword || '', res)
    return resp.data
  }

  async function searchKbOptions(query: string) {
    kbSelectProps.loading = true
    try {
      const { data: resp } = await kbApi.search({ title: query }, { current: 1, size: 100 })
      const list: SelectOpt[] = (resp?.records || []).map((item) => ({
        label: item.title,
        value: item.uuid,
      }))
      list.forEach((opt) => kbLabelByUuid.set(opt.value, opt.label))
      const selected = getFieldsValue().kbUuid
      if (
        selected &&
        !list.some((opt) => opt.value === selected) &&
        kbLabelByUuid.has(selected)
      ) {
        list.unshift({ label: kbLabelByUuid.get(selected)!, value: selected })
      }
      kbSelectProps.options.splice(0, kbSelectProps.options.length, ...list)
    } finally {
      kbSelectProps.loading = false
    }
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, {
      uuid: record.uuid,
      title: record.title,
      brief: record.brief,
    })
  }

  async function handleDelete(record: Recordable) {
    await documentApi.deleteOne(record.uuid)
    reloadTable()
  }

  function confirmEditForm(e) {
    e.preventDefault()
    formBtnLoading.value = true
    formRef.value.validate(async (errors) => {
      if (!errors) {
        await documentApi.edit({ ...editFormParams })
        window['$message'].success(t('common.editSuccess'))
        setTimeout(() => {
          showEditModal.value = false
          reloadTable()
        })
      } else {
        window['$message'].error(t('common.fillCompleteInfo'))
      }
      formBtnLoading.value = false
    })
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    // 重置回调
  }

  onMounted(() => {
    if (initialKbUuid && initialKbTitle) {
      kbLabelByUuid.set(initialKbUuid, initialKbTitle)
      kbSelectProps.options.push({ label: initialKbTitle, value: initialKbUuid })
    }
    searchKbOptions('')
  })
</script>
