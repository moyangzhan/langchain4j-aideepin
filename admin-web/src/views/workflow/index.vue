<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: Data) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1400"
    />

    <n-modal
      v-model:show="showEditModal"
      :show-icon="false"
      preset="dialog"
      :title="editFormParams.label"
    >
      <n-form
        :model="editFormParams"
        :rules="newUserRules"
        ref="formRef"
        label-placement="left"
        :label-width="120"
        class="py-4"
      >
        <n-form-item :label="t('common.title')" path="title">
          <n-input :placeholder="t('common.title')" v-model:value="editFormParams.title" />
        </n-form-item>
        <n-form-item :label="t('common.isPublic')" path="isPublic">
          <n-switch v-model:value="editFormParams.isPublic" />
        </n-form-item>
        <n-form-item :label="t('common.isEnable')" path="isEnable">
          <n-switch v-model:value="editFormParams.isEnable" />
        </n-form-item>
        <n-form-item :label="t('common.description')" path="remark">
          <n-input
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 10 }"
            :placeholder="t('common.description')"
            v-model:value="editFormParams.remark"
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
  import { h, reactive, ref } from 'vue'
  import { BasicTable, TableAction } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import workflowApi from '@/api/workflow'
  import { getColumns, Data } from './columns'
  const columns = getColumns()
  import { type FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { t } from '@/locales'

  const newUserRules: FormRules = {
    title: {
      required: false,
      trigger: ['blur', 'input'],
      message: () => t('common.title'),
    },
  }

  const schemas: FormSchema[] = [
    {
      field: 'title',
      component: 'NInput',
      label: t('common.title'),
      componentProps: {
        placeholder: t('common.title'),
        onInput: (e: any) => {
          console.log(e)
        },
      },
    },
    {
      field: 'isPublic',
      component: 'NSelect',
      label: t('common.isPublic'),
      componentProps: {
        options: [
          {
            label: t('common.yes'),
            value: true,
          },
          {
            label: t('common.no'),
            value: false,
          },
        ],
        onUpdateValue: (e: any) => {
          console.log(e)
        },
      },
    },
    {
      field: 'isEnable',
      component: 'NSelect',
      label: t('common.isEnable'),
      componentProps: {
        options: [
          {
            label: t('common.yes'),
            value: true,
          },
          {
            label: t('common.no'),
            value: false,
          },
        ],
        onUpdateValue: (e: any) => {
          console.log(e)
        },
      },
    },
    {
      field: 'createTime',
      component: 'NDatePicker',
      label: t('common.createTime'),
      componentProps: {
        type: 'datetimerange',
        clearable: true,
      },
    },
    {
      field: 'updateTime',
      component: 'NDatePicker',
      label: t('common.updateTime'),
      componentProps: {
        type: 'datetimerange',
        clearable: true,
      },
    },
  ]

  const formRef: any = ref(null)
  const actionRef = ref()
  const dialog = useDialog()
  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.edit'),
    uuid: '',
    title: '',
    remark: '',
    isPublic: false,
    isEnable: false,
  })

  const actionColumn = reactive({
    width: 200,
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
          {
            label: t('common.disable'),
            onClick: handleDisable.bind(null, record),
            ifShow: () => {
              return record.isEnable
            },
          },
          {
            label: t('common.enable'),
            onClick: handleEnable.bind(null, record),
            ifShow: () => {
              return !record.isEnable
            },
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
                handleDel(record)
              },
              onNegativeClick: () => {
                console.log('已取消')
              },
            })
          }
        },
      })
    },
  })

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
  })

  const loadDataTable = async (res) => {
    const resp = await workflowApi.search({ ...getFieldsValue() }, res)
    return resp.data
  }

  function onCheckedRow(rowKeys) {
    console.log(rowKeys)
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function confirmEditForm(e) {
    e.preventDefault()
    formBtnLoading.value = true
    formRef.value.validate(async (errors) => {
      if (!errors) {
        await workflowApi.updateBaseInfo(editFormParams)
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

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    editFormParams.label = t('common.edit')
  }

  async function handleEnable(record: Recordable) {
    await workflowApi.setEnable({ uuid: record.uuid, isEnable: true })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDisable(record: Recordable) {
    await workflowApi.setEnable({ uuid: record.uuid, isEnable: false })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  function handleSubmit(values: Recordable) {
    console.log(values)
    reloadTable()
  }

  function handleReset(values: Recordable) {
    console.log(values)
  }

  async function handleDel(record: Recordable) {
    let { success } = await workflowApi.del(record.uuid)
    if (success) {
      window['$message'].success(t('common.operationSuccess'))
      reloadTable()
    }
  }
</script>

<style lang="less" scoped></style>
