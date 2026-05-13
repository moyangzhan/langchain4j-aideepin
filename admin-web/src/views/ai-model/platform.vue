<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />
    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: AiPlatformData) => row.name"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="800"
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
      style="width: 550px"
    >
      <n-form
        :model="editFormParams"
        :rules="newRecordRules"
        ref="formRef"
        label-placement="left"
        :label-width="150"
        class="py-4"
        style="overflow-y: auto; overflow-x: hidden; max-height: 600px; width: 500px"
      >
        <n-form-item :label="t('model.modelName')" path="name">
          <n-input
            :placeholder="t('model.modelNamePlaceholder')"
            v-model:value="editFormParams.name"
          />
        </n-form-item>
        <n-form-item :label="t('common.title')" path="title">
          <n-input :placeholder="t('common.title')" v-model:value="editFormParams.title" />
        </n-form-item>
        <n-form-item :label="t('model.platformBaseUrl')" path="baseUrl">
          <n-input
            :placeholder="t('model.platformBaseUrlPlaceholder')"
            v-model:value="editFormParams.baseUrl"
          />
        </n-form-item>
        <n-form-item :label="t('model.apiKey')" path="apiKey">
          <n-input
            :placeholder="t('model.apiKey')"
            v-model:value="editFormParams.apiKey"
            type="password"
            show-password-on="click"
          />
        </n-form-item>
        <n-form-item :label="t('model.secretKey')" path="secretKey">
          <n-input
            :placeholder="t('model.secretKey')"
            v-model:value="editFormParams.secretKey"
            type="password"
            show-password-on="click"
          />
        </n-form-item>
        <n-form-item
          :label="t('model.enableProxy')"
          path="isProxyEnable"
          :feedback="t('model.proxyFeedback')"
        >
          <n-radio-group v-model:value="editFormParams.isProxyEnable" name="rg1">
            <n-radio v-for="opt in YES_NO" :key="opt.value" :value="opt.value">
              {{ opt.label }}
            </n-radio>
          </n-radio-group>
        </n-form-item>
        <n-form-item
          :label="t('model.openaiCompatible')"
          path="isOpenaiApiCompatible"
          :feedback="t('model.openaiCompatibleFeedback')"
        >
          <n-radio-group v-model:value="editFormParams.isOpenaiApiCompatible" name="rg1">
            <n-radio v-for="opt in YES_NO" :key="opt.value" :value="opt.value">
              {{ opt.label }}
            </n-radio>
          </n-radio-group>
        </n-form-item>
        <n-form-item :label="t('model.remark')" path="remark">
          <n-input
            type="textarea"
            :placeholder="t('model.remarkPlaceholder')"
            v-model:value="editFormParams.remark"
          />
        </n-form-item>
      </n-form>
      <template #action>
        <n-space>
          <n-button @click="() => (showEditModal = false)">{{ t('common.cancel') }}</n-button>
          <n-button type="info" :loading="formBtnLoading" @click="confirmForm">{{
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
  import { FormSchema, useForm } from '@/components/Form/index'
  import api from '@/api/modelPlatform'
  import { AiPlatformData, getColumns } from './platformColumns'
  const columns = getColumns()
  import { PlusOutlined } from '@vicons/antd'
  import { getYesNo, getDefaultModelPlatforms } from '@/utils/constants'
  const YES_NO = getYesNo()
  const DEFAULT_MODEL_PLATFORMS = getDefaultModelPlatforms()
  import { type FormRules, useDialog } from 'naive-ui'
  import { t } from '@/locales'

  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    id: '0',
    name: '',
    title: '',
    baseUrl: '',
    apiKey: '',
    secretKey: '',
    isProxyEnable: false,
    isOpenaiApiCompatible: false,
    remark: '',
  })
  const dialog = useDialog()
  const formRef: any = ref(null)
  const newRecordRules: FormRules = {
    name: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('model.nameRequired'),
    },
    title: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('common.title'),
    },
  }

  const actionRef = ref()
  const actionColumn = reactive({
    width: 160,
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
            ifShow: () => {
              return !DEFAULT_MODEL_PLATFORMS.find((item) => item.value == record.name)
            },
          },
        ],
        select: (key) => {
          if (key === 'delete') {
            dialog.warning({
              title: t('common.tip'),
              content: `${t('model.deletePlatformConfirmPrefix')}${record.name}${t(
                'model.deletePlatformConfirmSuffix'
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

  const schemas: FormSchema[] = [
    {
      field: 'name',
      component: 'NInput',
      label: t('model.modelName'),
      componentProps: {
        placeholder: t('model.modelNamePlaceholder'),
      },
    },
    {
      field: 'title',
      component: 'NInput',
      label: t('common.title'),
      componentProps: {
        placeholder: t('common.title'),
      },
    },
  ]

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
  })

  const loadDataTable = async () => {
    const resp = await api.search(
      { ...getFieldsValue() },
      {
        current: 1,
        size: 100,
      }
    )
    return resp.data
  }

  function addTable() {
    showEditModal.value = true
    editFormParams.label = t('common.create')
    editFormParams.id = ''
  }

  function onCheckedRow(rowKeys) {
    console.log(rowKeys)
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function confirmForm(e) {
    e.preventDefault()
    formBtnLoading.value = true
    formRef.value.validate(async (errors) => {
      if (!errors) {
        if (editFormParams.id === '') {
          await api.addOne(editFormParams)
        } else {
          await api.edit(editFormParams)
        }
        window['$message'].success(
          editFormParams.id === '' ? t('common.createSuccess') : t('common.editSuccess')
        )
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
    console.log(values)
    reloadTable()
  }

  function handleReset(values: Recordable) {
    console.log(values)
  }

  async function handleDelete(record: Recordable) {
    await api.deleteOne(record.uuid)
    window['$message'].info(t('common.deleteSuccess'))
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    editFormParams.label = t('common.edit')
  }
</script>

<style lang="less" scoped></style>
