<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: McpInfo) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="1400"
    />

    <n-modal
      v-model:show="showEditModal"
      :show-icon="false"
      preset="card"
      :title="editFormParams.label"
      class="max-w-[1000px] edit-modal"
    >
      <div class="h-[90%] max-h-[700px] overflow-y-auto">
        <n-form
          :model="editFormParams"
          :rules="newDataRules"
          ref="formRef"
          label-placement="top"
          :label-width="120"
          class="py-4"
        >
          <n-form-item :label="t('common.title')" path="title">
            <n-input
              :placeholder="t('mcp.titlePlaceholder')"
              v-model:value="editFormParams.title"
            />
          </n-form-item>
          <n-form-item :label="t('mcp.transportType')" path="transportType">
            <n-radio-group v-model:value="editFormParams.transportType" name="rg1">
              <n-radio v-for="opt in mcpTransportType" :key="opt.value" :value="opt.value">
                {{ opt.label }}
              </n-radio>
            </n-radio-group>
          </n-form-item>
          <n-form-item :label="t('mcp.installType')" path="installType">
            <n-radio-group v-model:value="editFormParams.installType" name="rg2">
              <n-radio v-for="opt in mcpInstallType" :key="opt.value" :value="opt.value">
                {{ opt.label }}
              </n-radio>
            </n-radio-group>
          </n-form-item>
          <n-form-item
            v-if="editFormParams.transportType === 'sse'"
            :label="t('mcp.sseUrl')"
            path="sseUrl"
          >
            <n-input
              :placeholder="t('mcp.sseUrlPlaceholder')"
              v-model:value="editFormParams.sseUrl"
            />
          </n-form-item>
          <n-form-item
            v-if="editFormParams.transportType === 'sse'"
            :label="t('mcp.sseTimeout')"
            path="sseTimeout"
          >
            <n-input-number v-model:value="editFormParams.sseTimeout" :min="1" />
          </n-form-item>
          <n-form-item
            v-if="editFormParams.transportType === 'stdio'"
            :label="t('mcp.stdioCommand')"
            path="stdioCommand"
          >
            <n-input
              :placeholder="t('mcp.stdioCommandPlaceholder')"
              v-model:value="editFormParams.stdioCommand"
            />
          </n-form-item>
          <n-form-item
            v-if="editFormParams.transportType === 'stdio'"
            :label="t('mcp.stdioArg')"
            path="stdioArg"
          >
            <n-input
              :placeholder="t('mcp.stdioArgPlaceholder')"
              v-model:value="editFormParams.stdioArg"
            />
          </n-form-item>
          <n-form-item :label="t('mcp.website')" path="website">
            <n-input
              :placeholder="t('mcp.websitePlaceholder')"
              v-model:value="editFormParams.website"
            />
          </n-form-item>
          <n-form-item :label="t('mcp.presetParams')" path="presetParams">
            <n-table :single-line="false">
              <thead>
                <tr>
                  <th>
                    {{ t('mcp.paramName') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.paramNameTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th>
                    {{ t('mcp.paramTitle') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.paramTitleTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th>{{ t('mcp.paramValue') }}</th>
                  <th class="flex justify-center">
                    {{ t('mcp.sensitiveInfo') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.sensitiveInfoTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th>{{ t('common.action') }}</th>
                </tr>
              </thead>
              <tbody>
                <tr
                  v-for="(presetParam, idx) in editFormParams.presetParams"
                  :key="'preset_' + idx"
                >
                  <td class="max-w-[200px]">
                    <n-input
                      v-model:value="presetParam.name"
                      :placeholder="t('mcp.paramNameTip')"
                    />
                  </td>
                  <td>
                    <n-input
                      v-model:value="presetParam.title"
                      :placeholder="t('mcp.paramTitlePlaceholder')"
                    />
                  </td>
                  <td>
                    <n-input
                      v-model:value="presetParam.value"
                      class="flex-1"
                      :placeholder="t('mcp.paramValuePlaceholder')"
                    />
                  </td>
                  <td class="flex justify-center">
                    <n-switch v-model:value="presetParam.require_encrypt" />
                  </td>
                  <td>
                    <n-icon size="18" class="mx-2 cursor-pointer" @click="removePresetParam(idx)">
                      <DeleteOutlined />
                    </n-icon>
                  </td>
                </tr>
              </tbody>
              <tfoot>
                <tr>
                  <td colspan="4" class="flex">
                    <n-button type="primary" dashed @click="addPresetParam">
                      {{ t('mcp.addParam') }}
                    </n-button>
                  </td>
                </tr>
              </tfoot>
            </n-table>
          </n-form-item>
          <n-form-item :label="t('mcp.paramConfigNote')" path="customizedParamDefinitions">
            <n-table :single-line="false">
              <thead>
                <tr>
                  <th>
                    {{ t('mcp.paramName') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.paramNameTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th>
                    {{ t('mcp.paramTitle') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.paramTitleTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th class="flex justify-center">
                    {{ t('mcp.sensitiveInfo') }}
                    <n-tooltip trigger="hover">
                      <template #trigger>
                        <n-icon>
                          <QuestionCircleOutlined />
                        </n-icon>
                      </template>
                      <span>{{ t('mcp.sensitiveInfoTip') }}</span>
                    </n-tooltip>
                  </th>
                  <th>{{ t('common.action') }}</th>
                </tr>
              </thead>
              <tbody>
                <tr
                  v-for="(uninitParam, idx) in editFormParams.customizedParamDefinitions"
                  :key="'definition_' + idx"
                >
                  <td class="max-w-[200px]">
                    <n-input
                      v-model:value="uninitParam.name"
                      class="flex-1"
                      :placeholder="t('mcp.paramNameTip')"
                    />
                  </td>
                  <td>
                    <n-input
                      v-model:value="uninitParam.title"
                      class="flex-1"
                      :placeholder="t('mcp.paramTitlePlaceholder')"
                    />
                  </td>
                  <td class="flex justify-center">
                    <n-switch v-model:value="uninitParam.require_encrypt" />
                  </td>
                  <td>
                    <n-icon
                      size="18"
                      class="mx-2 cursor-pointer"
                      @click="removeParamDefinition(idx)"
                    >
                      <DeleteOutlined />
                    </n-icon>
                  </td>
                </tr>
              </tbody>
              <tfoot>
                <tr>
                  <td colspan="4" class="flex">
                    <n-button type="primary" dashed @click="addParamDefinition">
                      {{ t('mcp.addParamDefinition') }}
                    </n-button>
                  </td>
                </tr>
              </tfoot>
            </n-table>
          </n-form-item>
          <n-form-item :label="t('common.description')" path="remark">
            <n-input
              type="textarea"
              :autosize="{ minRows: 3, maxRows: 15 }"
              :placeholder="t('mcp.descriptionPlaceholder')"
              v-model:value="editFormParams.remark"
            />
          </n-form-item>
          <n-form-item :label="t('common.isEnable')" path="isEnable">
            <n-switch v-model:value="editFormParams.isEnable" />
          </n-form-item>
        </n-form>
      </div>
      <template #action>
        <div class="flex justify-end space-x-2">
          <n-button @click="() => (showEditModal = false)">{{ t('common.cancel') }}</n-button>
          <n-button type="info" :loading="formBtnLoading" @click="confirmEditForm">{{
            t('common.confirm')
          }}</n-button>
        </div>
      </template>
    </n-modal>
  </n-card>
</template>

<script lang="ts" setup>
  import { h, reactive, ref } from 'vue'
  import { BasicTable, TableAction } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import { QuestionCircleOutlined, DeleteOutlined } from '@vicons/antd'
  import mcpApi from '@/api/mcp'
  import { getColumns } from './columns'
  const columns = getColumns()
  import { McpInfo, McpSearchReq, McpCustomizedParamDefinition, PresetParam } from '/#/mcp'
  import { getMcpTransportType, getMcpInstallType } from '@/utils/constants'
  const mcpTransportType = getMcpTransportType()
  const mcpInstallType = getMcpInstallType()
  import type { FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { t } from '@/locales'

  const newDataRules: FormRules = {
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
        placeholder: t('mcp.titlePlaceholder'),
      },
    },
    {
      field: 'transportType',
      component: 'NSelect',
      label: t('mcp.transportType'),
      componentProps: {
        options: mcpTransportType,
      },
    },
    {
      field: 'installType',
      component: 'NSelect',
      label: t('mcp.installType'),
      componentProps: {
        options: mcpInstallType,
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
    transportType: 'sse',
    installType: 'remote',
    remark: '',
    isEnable: false,
    sseUrl: '',
    sseTimeout: 30,
    stdioCommand: '',
    stdioArg: '',
    website: '',
    presetParams: [] as PresetParam[],
    customizedParamDefinitions: [] as McpCustomizedParamDefinition[],
    repoUrl: '',
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
    const resp = await mcpApi.mcpSearch({ ...getFieldsValue() } as McpSearchReq, res)
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
          await mcpApi.mcpEdit(editFormParams)
          window['$message'].success(t('common.editSuccess'))
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
    await mcpApi.mcpSetEnable({ uuid: record.uuid, isEnable: true })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  async function handleDisable(record: Recordable) {
    await mcpApi.mcpSetEnable({ uuid: record.uuid, isEnable: false })
    window['$message'].success(t('common.operationSuccess'))
    reloadTable()
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    console.log(values)
  }

  async function handleDel(record: Recordable) {
    let { success } = await mcpApi.mcpDel(record.uuid)
    if (success) {
      window['$message'].success(t('common.operationSuccess'))
      reloadTable()
    }
  }

  function addParamDefinition() {
    editFormParams.customizedParamDefinitions.push({
      name: '',
      title: '',
      require_encrypt: false,
    })
  }

  function removeParamDefinition(idx: number) {
    editFormParams.customizedParamDefinitions.splice(idx, 1)
  }

  function addPresetParam() {
    editFormParams.presetParams.push({
      name: '',
      title: '',
      value: '',
      require_encrypt: false,
      encrypted: false,
    })
  }

  function removePresetParam(idx: number) {
    editFormParams.presetParams.splice(idx, 1)
  }
</script>
<style lang="less" scoped>
  .edit-modal {
    ::v-deep(.n-form-item-label__text) {
      font-weight: bold;
      border-left: 3px solid gray;
      padding-left: 0.2rem;
    }
  }
</style>
