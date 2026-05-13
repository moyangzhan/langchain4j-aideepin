<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: UserData) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1400"
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
    >
      <n-form
        :model="editFormParams"
        :rules="newUserRules"
        ref="formRef"
        label-placement="left"
        :label-width="120"
        class="py-4"
      >
        <n-form-item :label="t('common.name')" path="name">
          <n-input :placeholder="t('user.namePlaceholder')" v-model:value="editFormParams.name" />
        </n-form-item>
        <n-form-item :label="t('user.password')" path="password">
          <n-input
            :placeholder="t('user.passwordPlaceholder')"
            v-model:value="editFormParams.password"
          />
        </n-form-item>
        <n-form-item :label="t('user.dailyTokenQuota')" path="quotaByTokenDaily">
          <n-input-number v-model:value="editFormParams.quotaByTokenDaily" />
        </n-form-item>
        <n-form-item :label="t('user.monthlyTokenQuota')" path="quotaByTokenMonthly">
          <n-input-number v-model:value="editFormParams.quotaByTokenMonthly" />
        </n-form-item>
        <n-form-item :label="t('user.dailyRequestQuota')" path="quotaByRequestDaily">
          <n-input-number v-model:value="editFormParams.quotaByRequestDaily" />
        </n-form-item>
        <n-form-item :label="t('user.monthlyRequestQuota')" path="quotaByRequestMonthly">
          <n-input-number v-model:value="editFormParams.quotaByRequestMonthly" />
        </n-form-item>
        <n-form-item :label="t('user.dailyImageQuota')" path="quotaByImageDaily">
          <n-input-number v-model:value="editFormParams.quotaByImageDaily" />
        </n-form-item>
        <n-form-item :label="t('user.monthlyImageQuota')" path="quotaByImageMonthly">
          <n-input-number v-model:value="editFormParams.quotaByImageMonthly" />
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
  import userApi from '@/api/user'
  import { getColumns, UserData } from './columns'
  const columns = getColumns()
  import { PlusOutlined } from '@vicons/antd'
  import { type FormRules } from 'naive-ui'
  import { t } from '@/locales'

  const newUserRules: FormRules = {
    name: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('common.name'),
    },
  }

  const schemas: FormSchema[] = [
    {
      field: 'name',
      component: 'NInput',
      label: t('common.name'),
      componentProps: {
        placeholder: t('user.nameSearchPlaceholder'),
      },
    },
    {
      field: 'userStatus',
      component: 'NSelect',
      label: t('user.userStatus'),
      componentProps: {
        placeholder: t('user.statusPlaceholder'),
        options: [
          {
            label: t('user.statusPending'),
            value: 1,
          },
          {
            label: t('user.statusNormal'),
            value: 2,
          },
          {
            label: t('user.statusDisabled'),
            value: 3,
          },
        ],
        onUpdateValue: (e: any) => {},
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
    {
      field: 'isAdmin',
      component: 'NSelect',
      label: t('user.admin'),
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
        onUpdateValue: (e: any) => {},
      },
    },
  ]

  const formRef: any = ref(null)
  const actionRef = ref()

  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    uuid: '',
    name: '',
    email: '',
    password: '',
    quotaByTokenDaily: 0,
    quotaByTokenMonthly: 0,
    quotaByRequestDaily: 0,
    quotaByRequestMonthly: 0,
    quotaByImageDaily: 0,
    quotaByImageMonthly: 0,
    isAdmin: false,
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
              return record.userStatus === 'NORMAL'
            },
          },
          {
            label: t('common.enable'),
            onClick: handleEnable.bind(null, record),
            ifShow: () => {
              return record.userStatus === 'FREEZE'
            },
          },
        ],
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
    editFormParams.uuid = ''
    editFormParams.name = ''
    editFormParams.email = ''
    editFormParams.quotaByTokenDaily = 0
    editFormParams.quotaByTokenMonthly = 0
    editFormParams.quotaByRequestDaily = 0
    editFormParams.quotaByRequestMonthly = 0
    editFormParams.quotaByImageDaily = 0
    editFormParams.quotaByImageMonthly = 0
    editFormParams.isAdmin = false
  }

  const loadDataTable = async (res) => {
    const resp = await userApi.search({ ...getFieldsValue() }, res)
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
          if (editFormParams.uuid === '') {
            await userApi.addOne(editFormParams)
          } else {
            await userApi.edit(editFormParams)
          }
          window['$message'].success(
            editFormParams.uuid === '' ? t('common.createSuccess') : t('common.editSuccess')
          )
          setTimeout(() => {
            showEditModal.value = false
            reloadTable()
          })
        } else {
          window['$message'].error(t('common.fillCompleteInfo'))
        }
      } catch (error) {
        window['$message'].error(t('common.operationFailed'))
      } finally {
        formBtnLoading.value = false
      }
    })
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    editFormParams.label = t('common.edit')
  }

  async function handleEnable(record: Recordable) {
    await userApi.active(record.uuid)
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDisable(record: Recordable) {
    await userApi.freeze(record.uuid)
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    // 重置回调
  }
</script>

<style lang="less" scoped></style>
