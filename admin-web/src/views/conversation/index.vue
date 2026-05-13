<template>
  <n-card :bordered="false" class="proCard">
    <n-alert type="info" :show-icon="false" closable>
      {{ t('conversation.conversationIsRole') }}
    </n-alert>

    <BasicForm class="mt-3" @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: Conversation) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1300"
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
        :label-width="80"
        class="py-4"
      >
        <n-form-item :label="t('common.title')" path="title">
          <n-input
            :placeholder="t('conversation.titlePlaceholder')"
            v-model:value="editFormParams.title"
            maxlength="45"
            show-count
          />
        </n-form-item>
        <n-form-item :label="t('common.description')" path="password">
          <n-input
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 10 }"
            :placeholder="t('conversation.descriptionPlaceholder')"
            v-model:value="editFormParams.aiSystemMessage"
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
  import convApi from '@/api/conversation'
  import { getColumns } from './columns'
  const columns = getColumns()
  import { type FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { Conversation } from '/#/conversation'
  import { t } from '@/locales'

  const newUserRules: FormRules = {
    name: {
      required: false,
      trigger: ['blur', 'input'],
      message: () => t('common.name'),
    },
  }

  const schemas: FormSchema[] = [
    {
      field: 'title',
      component: 'NInput',
      label: t('common.title'),
      componentProps: {
        placeholder: t('conversation.titlePlaceholder'),
        onInput: (e: any) => {
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

  const dialog = useDialog()
  const formRef: any = ref(null)
  const actionRef = ref()

  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    uuid: '',
    title: '',
    aiSystemMessage: '',
    understandContextEnable: false,
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
        ],
        dropDownActions: [
          {
            label: t('common.delete'),
            key: 'deleteConv',
          },
        ],
        select: (key) => {
          if (key === 'deleteConv') {
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
    const resp = await convApi.searchConvs({ ...getFieldsValue() }, res)
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
        await convApi.editConv(editFormParams.uuid, editFormParams)
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

  async function handleDelete(record: Recordable) {
    await convApi.deleteConv(record.uuid)
    reloadTable()
  }

  function handleSubmit(values: Recordable) {
    console.log(values)
    reloadTable()
  }

  function handleReset(values: Recordable) {
    console.log(values)
  }
</script>

<style lang="less" scoped></style>
