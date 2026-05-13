<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: ComponentData) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1200"
    >
      <template #tableTitle>
        <n-button type="primary" @click="addTable">
          <template #icon>
            <n-icon>
              <PlusOutlined />
            </n-icon>
          </template>
          {{ t('common.create') }}
        </n-button>
      </template>
    </BasicTable>

    <n-modal
      v-model:show="showEditModal"
      :show-icon="false"
      preset="dialog"
      :title="editFormParams.label"
      class="min-w-[500px]"
    >
      <n-form
        :model="editFormParams"
        :rules="formRules"
        ref="formRef"
        label-placement="left"
        :label-width="100"
        class="py-4"
      >
        <n-form-item :label="t('common.name')" path="name">
          <div class="flex flex-col flex-grow">
            <n-input
              :placeholder="t('workflow.componentTitlePlaceholder')"
              v-model:value="editFormParams.name"
              :disabled="editFormParams.isEdit ? true : false"
            />
            <div class="text-sm italic text-gray-400">{{ t('workflow.componentNameTip1') }}</div>
            <div class="text-sm italic text-gray-400">{{ t('workflow.componentNameTip2') }}</div>
          </div>
        </n-form-item>
        <n-form-item :label="t('common.title')" path="title">
          <n-input :placeholder="t('common.title')" v-model:value="editFormParams.title" />
        </n-form-item>
        <n-form-item :label="t('workflow.componentDisplayOrder')" path="displayOrder">
          <div class="flex flex-col flex-grow">
            <n-input-number v-model:value="editFormParams.displayOrder" />
            <div class="text-sm italic text-gray-400">
              {{ t('workflow.componentDisplayOrderTip') }}
            </div>
          </div>
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
  import { getColumns, ComponentData } from './columns'
  const columns = getColumns()
  import { PlusOutlined } from '@vicons/antd'
  import { type FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { t } from '@/locales'

  //'Start', 'End', 'Answer', 'KnowledgeRetrieval', 'Switcher', 'Classifier', 'Template', 'DocumentExtractor', 'KeywordExtractor'
  const nonDeletableNodes = ['Start', 'End']

  const formRules: FormRules = {
    name: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('common.name'),
    },
    title: {
      required: true,
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
      },
    },
  ]

  const formRef: any = ref(null)
  const actionRef = ref()
  const dialog = useDialog()
  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    isEdit: false,
    uuid: '',
    name: '',
    title: '',
    remark: '',
    displayOrder: 0,
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
              return record.isEnable && !nonDeletableNodes.includes(record.name)
            },
          },
          {
            label: t('common.enable'),
            onClick: handleEnable.bind(null, record),
            ifShow: () => {
              return !record.isEnable && !nonDeletableNodes.includes(record.name)
            },
          },
        ],
        dropDownActions: [
          {
            label: t('common.delete'),
            key: 'delete',
            ifShow: () => {
              return !nonDeletableNodes.includes(record.name)
            },
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
                // 取消删除
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

  function addTable() {
    showEditModal.value = true
    editFormParams.label = t('common.create')
    editFormParams.isEdit = false
    editFormParams.uuid = ''
    editFormParams.name = ''
    editFormParams.title = ''
    editFormParams.remark = ''
    editFormParams.isEnable = false
  }

  const loadDataTable = async (res) => {
    const resp = await workflowApi.componentSearch({ ...getFieldsValue() }, res)
    return resp.data
  }

  function onCheckedRow(rowKeys) {
    // 选中行回调
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function confirmEditForm(e) {
    e.preventDefault()
    formBtnLoading.value = true
    formRef.value.validate(async (errors) => {
      try {
        if (!errors) {
          const { success } = await workflowApi.componentAddOrUpdate(editFormParams)
          if (success) {
            window['$message'].success(
              editFormParams.isEdit ? t('common.editSuccess') : t('common.createSuccess')
            )
            setTimeout(() => {
              showEditModal.value = false
              reloadTable()
            })
          } else {
            window['$message'].error(t('common.operationFailed'))
          }
        } else {
          window['$message'].error(t('common.fillCompleteInfo'))
        }
      } catch (e) {
        console.error(e)
        formBtnLoading.value = false
      }
    })
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    editFormParams.label = t('common.edit')
    editFormParams.isEdit = true
  }

  async function handleEnable(record: Recordable) {
    await workflowApi.componentEnable({ uuid: record.uuid, isEnable: true })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDisable(record: Recordable) {
    await workflowApi.componentEnable({ uuid: record.uuid, isEnable: false })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDel(record: Recordable) {
    let { success } = await workflowApi.componentDel({ uuid: record.uuid })
    if (success) {
      window['$message'].success(t('common.operationSuccess'))
      reloadTable()
    }
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    // 重置回调
  }
</script>

<style lang="less" scoped></style>
