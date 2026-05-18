<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: CharacterPreset) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1300"
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
        :rules="formRules"
        ref="formRef"
        label-placement="left"
        :label-width="100"
        class="py-4"
      >
        <n-form-item :label="t('common.title')" path="title">
          <n-input
            :placeholder="t('character.titlePlaceholder')"
            v-model:value="editFormParams.title"
            maxlength="45"
            show-count
          />
        </n-form-item>
        <n-form-item :label="t('character.presetType')" path="type">
          <n-select
            :placeholder="t('character.presetTypePlaceholder')"
            v-model:value="editFormParams.type"
            :options="presetTypeOptions"
            clearable
          />
        </n-form-item>
        <n-form-item :label="t('common.description')" path="remark">
          <n-input
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 10 }"
            :placeholder="t('character.descriptionPlaceholder')"
            v-model:value="editFormParams.remark"
          />
        </n-form-item>
        <n-form-item :label="t('character.aiSystemMessage')" path="aiSystemMessage">
          <n-input
            type="textarea"
            :autosize="{ minRows: 3, maxRows: 10 }"
            :placeholder="t('character.aiSystemMessagePlaceholder')"
            v-model:value="editFormParams.aiSystemMessage"
          />
        </n-form-item>
        <n-form-item :label="t('character.kbTitle')" path="kbTitle">
          <n-input
            :placeholder="t('character.kbTitlePlaceholder')"
            v-model:value="editFormParams.kbTitle"
            maxlength="100"
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
  import characterApi from '@/api/conversation'
  import { getColumns } from './columns'
  const columns = getColumns()
  import { CharacterPreset } from '/#/conversation'
  import { PlusOutlined } from '@vicons/antd'
  import { type FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { t } from '@/locales'

  const formRules: FormRules = {
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
        placeholder: t('character.titlePlaceholder'),
      },
    },
    {
      field: 'remark',
      component: 'NInput',
      label: t('common.description'),
      componentProps: {
        placeholder: t('character.descriptionPlaceholder'),
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

  const presetTypeOptions = [
    { label: () => t('character.presetTypeTechnology'), value: 'technology' },
    { label: () => t('character.presetTypeCreative'), value: 'creative' },
    { label: () => t('character.presetTypeEducation'), value: 'education' },
    { label: () => t('character.presetTypeBusiness'), value: 'business' },
    { label: () => t('character.presetTypeProfessional'), value: 'professional' },
    { label: () => t('character.presetTypeDesign'), value: 'design' },
    { label: () => t('character.presetTypeMarketing'), value: 'marketing' },
    { label: () => t('character.presetTypeService'), value: 'service' },
    { label: () => t('character.presetTypeAdministration'), value: 'administration' },
    { label: () => t('character.presetTypeUtility'), value: 'utility' },
  ]
  const actionRef = ref()

  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    uuid: '',
    title: '',
    remark: '',
    aiSystemMessage: '',
    kbTitle: '',
    type: '',
  })

  const actionColumn = reactive({
    width: 120,
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

  const [register, { getFieldsValue }] = useForm({
    gridProps: { cols: '1 s:1 m:2 l:3 xl:4 2xl:4' },
    labelWidth: 120,
    schemas,
  })

  function addTable() {
    showEditModal.value = true
    editFormParams.label = t('common.create')
    editFormParams.uuid = ''
    editFormParams.title = ''
    editFormParams.remark = ''
    editFormParams.aiSystemMessage = ''
    editFormParams.kbTitle = ''
    editFormParams.type = ''
  }

  const loadDataTable = async (res) => {
    const resp = await characterApi.searchPresetCharacters({ ...getFieldsValue() }, res)
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
      if (!errors) {
        if (editFormParams.uuid === '') {
          await characterApi.addPresetCharacter(editFormParams)
        } else {
          await characterApi.editPresetCharacter(editFormParams.uuid, editFormParams)
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
      formBtnLoading.value = false
    })
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    editFormParams.label = t('common.edit')
  }

  function handleDelete(record: Recordable) {
    characterApi.deletePresetCharacter(record.uuid)
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
