<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: AiModelData) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1800"
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
        :label-width="160"
        class="p-2"
        style="overflow-y: auto; overflow-x: hidden; max-height: 600px; width: 500px"
      >
        <n-form-item :label="t('model.modelName')" path="name">
          <n-input
            :placeholder="t('model.modelNamePlaceholder')"
            v-model:value="editFormParams.name"
          />
        </n-form-item>
        <n-form-item :label="t('model.modelTitle')" path="title">
          <n-input
            :placeholder="t('model.modelTitlePlaceholder')"
            v-model:value="editFormParams.title"
          />
        </n-form-item>
        <n-form-item :label="t('model.type')" path="type">
          <n-select
            :placeholder="t('model.typePlaceholder')"
            :options="MODEL_TYPES"
            v-model:value="editFormParams.type"
          />
        </n-form-item>
        <n-form-item :label="t('model.platform')" path="platform">
          <n-select
            :placeholder="t('model.platformPlaceholder')"
            :options="allPlatforms"
            v-model:value="editFormParams.platform"
          />
        </n-form-item>
        <n-form-item :label="t('model.contextWindow')" path="contextWindow">
          <n-input-number
            :placeholder="t('model.contextWindowPlaceholder')"
            v-model:value="editFormParams.contextWindow"
          />
        </n-form-item>
        <n-form-item :label="t('model.maxInputTokens')" path="maxInputTokens">
          <n-input-number
            :placeholder="t('model.maxInputTokensPlaceholder')"
            v-model:value="editFormParams.maxInputTokens"
          />
        </n-form-item>
        <n-form-item :label="t('model.maxOutputTokens')" path="maxOutputTokens">
          <n-input-number
            :placeholder="t('model.maxOutputTokensPlaceholder')"
            v-model:value="editFormParams.maxOutputTokens"
          />
        </n-form-item>
        <n-form-item :label="t('model.inputType')" path="inputTypeList">
          <n-select
            multiple
            :placeholder="t('model.inputTypePlaceholder')"
            :options="MODEL_INPUT_TYPES"
            v-model:value="editFormParams.inputTypeList"
          />
        </n-form-item>
        <n-form-item :label="t('model.responseFormat')" path="responseFormatTypeList">
          <n-select
            multiple
            :placeholder="t('model.responseFormatPlaceholder')"
            :options="MODEL_RESPONSE_FORMAT_TYPES"
            v-model:value="editFormParams.responseFormatTypeList"
          />
        </n-form-item>
        <n-form-item :label="t('model.isReasoner')" path="isReasoner">
          <n-switch v-model:value="editFormParams.isReasoner" />
        </n-form-item>
        <n-form-item :label="t('model.isThinkingClosable')" path="isThinkingClosable">
          <n-switch v-model:value="editFormParams.isThinkingClosable" />
        </n-form-item>
        <n-form-item :label="t('model.isSupportWebSearch')" path="isSupportWebSearch">
          <n-switch v-model:value="editFormParams.isSupportWebSearch" />
        </n-form-item>
        <n-form-item :label="t('model.personalSetting')" path="setting">
          <n-input
            type="textarea"
            :placeholder="t('model.personalSetting')"
            v-model:value="editFormParams.setting"
          />
        </n-form-item>
        <n-form-item :label="t('model.remark')" path="remark">
          <n-input
            type="textarea"
            :placeholder="t('model.remarkPlaceholder')"
            v-model:value="editFormParams.remark"
          />
        </n-form-item>
        <n-form-item :label="t('model.properties')" path="properties">
          <JsonEditorVue
            v-model="editFormParams.properties"
            :main-menu-bar="false"
            :mode="Mode.text"
            style="width: 100%; height: 200px"
          />
        </n-form-item>
      </n-form>
      <template #action>
        <n-space>
          <n-button @click="() => (showEditModal = false)">{{ t('common.cancel') }}</n-button>
          <n-button type="info" :loading="formBtnLoading" @click="confirmForm">
            {{ t('common.confirm') }}
          </n-button>
        </n-space>
      </template>
    </n-modal>
  </n-card>
</template>

<script lang="ts" setup>
  import { h, onMounted, reactive, ref, computed } from 'vue'
  import { BasicTable, TableAction } from '@/components/Table'
  import { BasicForm, useForm } from '@/components/Form/index'
  import api from '@/api/aiModel'
  import platformApi from '@/api/modelPlatform'
  import {
    getModelTypes,
    getModelInputTypes,
    getModelResponseFormatTypes,
    getDefaultModelPlatforms,
  } from '@/utils/constants'
  import { getColumns } from './columns'
  const columns = getColumns()
  const MODEL_TYPES = getModelTypes()
  const MODEL_INPUT_TYPES = getModelInputTypes()
  const MODEL_RESPONSE_FORMAT_TYPES = getModelResponseFormatTypes()
  const allPlatforms = getDefaultModelPlatforms()
  import { PlusOutlined } from '@vicons/antd'
  import { AiModelData } from '/#/aiModel'
  import type { FormItemRule, FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import JsonEditorVue from 'json-editor-vue'
  import { t } from '@/locales'

  enum Mode {
    text = 'text',
    tree = 'tree',
  }

  const newRecordRules: FormRules = {
    name: {
      required: false,
      trigger: ['blur', 'input'],
      message: () => t('model.nameRequired'),
    },
    type: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('model.typeRequired'),
    },
    platform: {
      required: true,
      trigger: ['blur', 'input'],
      message: () => t('model.platformRequired'),
    },
    inputTypeList: {
      required: true,
      trigger: ['blur'],
      message: () => t('model.inputTypePlaceholder'),
      validator(rule: FormItemRule, value: string[]) {
        if (value.length === 0) {
          return new Error(t('model.inputTypeRequired'))
        }
        return true
      },
    },
    responseFormatTypeList: {
      required: true,
      trigger: ['blur'],
      message: () => t('model.responseFormatPlaceholder'),
      validator(rule: FormItemRule, value: string[]) {
        if (value.length === 0) {
          return new Error(t('model.responseFormatRequired'))
        }
        return true
      },
    },
  }

  const dialog = useDialog()
  const formRef: any = ref(null)
  const actionRef = ref()
  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    label: t('common.create'),
    id: '0',
    name: '',
    title: '',
    type: '',
    platform: '',
    contextWindow: 0,
    maxInputTokens: 0,
    maxOutputTokens: 0,
    inputTypeList: [],
    responseFormatTypeList: [],
    isReasoner: false,
    isThinkingClosable: false,
    isSupportWebSearch: false,
    setting: '',
    remark: '',
    properties: null as Record<string, any> | null,

    inputTypes: '',
    responseFormatTypes: '',
  })

  const actionColumn = reactive({
    width: 300,
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
            label: t('common.enable'),
            onClick: handleEnable.bind(null, record),
            ifShow: () => {
              return !record.isEnable
            },
          },
          {
            label: t('common.disable'),
            onClick: handleDisable.bind(null, record),
            ifShow: () => {
              return record.isEnable
            },
          },
          {
            label: t('model.setFree'),
            onClick: handleFree.bind(null, record, true),
            ifShow: () => {
              return !record.isFree
            },
          },
          {
            label: t('model.setPaid'),
            onClick: handleFree.bind(null, record, false),
            ifShow: () => {
              return record.isFree
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
              content: `${t('model.deleteModelConfirmPrefix')} ${record.name} ${t(
                'model.deleteModelConfirmSuffix'
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
  })

  function addTable() {
    showEditModal.value = true
    editFormParams.label = t('common.create')
    editFormParams.id = ''
  }

  const loadDataTable = async (res) => {
    const resp = await api.search({ ...getFieldsValue() }, res)
    if (resp.data.records) {
      resp.data.records.forEach((item: AiModelData) => {
        item.inputTypeList = item.inputTypes.split(',')
        item.responseFormatTypeList = item.responseFormatTypes.split(',')
      })
    }
    return resp.data
  }

  const loadPlatforms = async () => {
    const resp = await platformApi.search({}, { current: 1, size: 100 })
    allPlatforms.length = 0
    resp.data.records.forEach((item) => {
      allPlatforms.push({ label: item.title, value: item.name })
    })
  }

  function onCheckedRow(rowKeys) {
    // 选中行回调
  }

  function reloadTable() {
    actionRef.value.reload()
  }

  function confirmForm(e) {
    e.preventDefault()
    formBtnLoading.value = true
    formRef.value.validate(async (errors) => {
      try {
        if (!errors) {
          editFormParams.inputTypes = editFormParams.inputTypeList.join(',')
          editFormParams.responseFormatTypes = editFormParams.responseFormatTypeList.join(',')
          // 处理 properties：如果是字符串则解析为对象
          let propertiesValue: Record<string, any> | null = editFormParams.properties
          if (typeof propertiesValue === 'string' && (propertiesValue as string).trim()) {
            try {
              propertiesValue = JSON.parse(propertiesValue as string)
            } catch {
              propertiesValue = null
            }
          }
          const submitData = {
            ...editFormParams,
            properties: propertiesValue || null,
          }
          if (editFormParams.id === '') {
            await api.addOne(submitData)
          } else {
            await api.edit(submitData)
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
      } catch (error) {
        window['$message'].error(t('common.operationFailed'))
      } finally {
        formBtnLoading.value = false
      }
    })
  }

  async function handleEnable(record: Recordable) {
    await api.enable(record.id)
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDisable(record: Recordable) {
    await api.disable(record.id)
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleFree(record: Recordable, isFree: boolean) {
    await api.edit({ id: record.id, isFree })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  function handleEdit(record: Recordable) {
    showEditModal.value = true
    Object.assign(editFormParams, record)
    if (!editFormParams.setting) {
      editFormParams.setting = ''
    }
    // 处理 properties 字段（后端返回的可能是对象或字符串）
    if (record.properties) {
      if (typeof record.properties === 'string') {
        try {
          editFormParams.properties = JSON.parse(record.properties)
        } catch {
          editFormParams.properties = null
        }
      } else {
        editFormParams.properties = record.properties
      }
    } else {
      editFormParams.properties = null
    }
    editFormParams.label = t('common.edit')
  }

  async function handleDelete(record: Recordable) {
    await api.deleteOne(record.uuid)
    window['$message'].info(t('common.deleteSuccess'))
  }

  function handleSubmit(_values: Recordable) {
    reloadTable()
  }

  function handleReset(_values: Recordable) {
    // 重置回调
  }

  onMounted(() => {
    loadPlatforms()
  })
</script>

<style lang="less" scoped></style>
