<template>
  <n-card :bordered="false" class="proCard">
    <BasicForm @register="register" @submit="handleSubmit" @reset="handleReset" />

    <BasicTable
      :columns="columns"
      :request="loadDataTable"
      :row-key="(row: KbInfoData) => row.id"
      ref="actionRef"
      :actionColumn="actionColumn"
      @update:checked-row-keys="onCheckedRow"
      :scroll-x="2000"
    />

    <n-modal
      v-model:show="showEditModal"
      :show-icon="false"
      preset="dialog"
      :title="t('common.edit')"
      class="min-w-[600px]"
    >
      <n-form
        :model="editFormParams"
        :rules="newRecordRules"
        ref="formRef"
        label-placement="left"
        :label-width="150"
        class="py-4"
      >
        <n-form-item :label="t('common.title')" path="title">
          <n-input
            :placeholder="t('knowledgeBase.titlePlaceholder')"
            v-model:value="editFormParams.title"
          />
        </n-form-item>
        <n-form-item :label="t('common.description')" path="remark">
          <n-input
            type="textarea"
            :placeholder="t('common.description')"
            v-model:value="editFormParams.remark"
          />
        </n-form-item>
        <n-form-item :label="t('common.isPublic')" path="isPublic">
          <n-switch v-model:value="editFormParams.isPublic" />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.isStrict')" path="isStrict">
          <n-switch v-model:value="editFormParams.isStrict" />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.ingestMaxOverlap')" path="ingestMaxOverlap">
          <n-input-number
            :placeholder="t('knowledgeBase.ingestMaxOverlap')"
            v-model:value="editFormParams.ingestMaxOverlap"
            class="flex-grow"
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.ingestModelName')" path="ingestModelName">
          <n-select
            :placeholder="t('knowledgeBase.ingestModelNamePlaceholder')"
            :options="aiModelOpts"
            v-model:value="editFormParams.ingestModelId"
            filterable
            clearable
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.tokenEstimator')" path="ingestTokenEstimator">
          <n-select
            :placeholder="t('knowledgeBase.tokenEstimatorPlaceholder')"
            :options="tokenEstimatorOpts"
            v-model:value="editFormParams.ingestTokenEstimator"
            clearable
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.retrieveMaxResults')" path="retrieveMaxResults">
          <n-input-number
            :placeholder="t('knowledgeBase.retrieveMaxResults')"
            v-model:value="editFormParams.retrieveMaxResults"
            class="flex-grow"
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.retrieveMinScore')" path="retrieveMinScore">
          <n-input-number
            :placeholder="t('knowledgeBase.retrieveMinScore')"
            v-model:value="editFormParams.retrieveMinScore"
            :precision="1"
            :min="0"
            :max="1"
            class="flex-grow"
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.queryLlmTemperature')" path="queryLlmTemperature">
          <n-input-number
            :placeholder="t('knowledgeBase.queryLlmTemperature')"
            v-model:value="editFormParams.queryLlmTemperature"
            :precision="1"
            :min="0"
            :max="1"
            class="flex-grow"
          />
        </n-form-item>
        <n-form-item :label="t('knowledgeBase.querySystemMessage')" path="querySystemMessage">
          <n-input
            type="textarea"
            :placeholder="t('knowledgeBase.querySystemMessage')"
            v-model:value="editFormParams.querySystemMessage"
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
  import { h, onMounted, reactive, ref } from 'vue'
  import { BasicTable, TableAction } from '@/components/Table'
  import { BasicForm, FormSchema, useForm } from '@/components/Form/index'
  import api from '@/api/knowledgeBase'
  import aiModelApi from '@/api/aiModel'
  import { getColumns, KbInfoData } from './columns'
  const columns = getColumns()
  import { type FormRules } from 'naive-ui'
  import { useDialog } from 'naive-ui'
  import { t } from '@/locales'

  interface SelectOpt {
    label: string
    value: number
  }
  const showEditModal = ref(false)
  const formBtnLoading = ref(false)
  const editFormParams = reactive({
    uuid: '',
    title: '',
    remark: '',
    isPublic: false,
    isStrict: false,
    ingestMaxOverlap: 0,
    ingestModelId: 0,
    ingestTokenEstimator: '',
    retrieveMaxResults: 0,
    retrieveMinScore: 0.0,
    queryLlmTemperature: 0.0,
    querySystemMessage: '',
  })
  const publicOpts = [
    {
      label: t('common.yes'),
      value: 1,
    },
    {
      label: t('common.no'),
      value: 0,
    },
  ]
  const tokenEstimatorOpts = [
    { label: 'OpenAI', value: 'openai' },
    { label: 'HuggingFace', value: 'huggingface' },
    { label: 'Qwen', value: 'qwen' },
  ]
  const aiModelOpts = ref<SelectOpt[]>([])
  const dialog = useDialog()
  const formRef: any = ref(null)
  const newRecordRules: FormRules = {
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
        placeholder: t('knowledgeBase.titlePlaceholder'),
      },
    },
    {
      field: 'ownerName',
      component: 'NInput',
      label: t('knowledgeBase.ownerName'),
      componentProps: {
        placeholder: t('knowledgeBase.ownerNamePlaceholder'),
      },
    },
    {
      field: 'isPublic',
      component: 'NSelect',
      label: t('common.isPublic'),
      componentProps: {
        options: publicOpts,
      },
    },
    {
      field: 'createDate',
      component: 'NDatePicker',
      label: t('common.createTime'),
      componentProps: {
        type: 'datetimerange',
        'value-format': 'yyyy.MM.dd HH:mm:ss',
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

  const loadDataTable = async (res) => {
    const resp = await api.search({ ...getFieldsValue() }, res)
    return resp.data
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
      if (!errors) {
        await api.edit({ ...editFormParams, isPublic: editFormParams.isPublic })
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
  }

  async function handleDelete(record: Recordable) {
    await api.deleteOne(record.uuid)
    window['$message'].info(t('common.deleteSuccess'))
    reloadTable()
  }

  function handleSubmit(values: Recordable) {
    reloadTable()
  }

  function handleReset(values: Recordable) {
    // 重置回调
  }

  onMounted(async () => {
    if (aiModelOpts.value.length > 0) {
      return
    }
    const { data: resp } = await aiModelApi.search(
      { isEnable: true, type: 'text' },
      { current: 1, size: 100 }
    )
    if (resp && resp.records) {
      resp.records.forEach((item) => {
        aiModelOpts.value.push({ label: item.name, value: item.id })
      })
    }
  })
</script>

<style lang="less" scoped></style>
