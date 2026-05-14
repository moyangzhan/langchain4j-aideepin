<script setup lang="ts">
import { NButton, NCollapse, NCollapseItem, NInput, NInputNumber, NSelect, NSwitch, NTable, NTag, NTooltip } from 'naive-ui'
import JsonEditorVue from 'json-editor-vue'
import NodePropertyInput from '../NodePropertyInput.vue'
import { SvgIcon } from '@/components/common'
import { t } from '@/locales'
const props = defineProps<Props>()
enum Mode {
  text = 'text',
  tree = 'tree',
  table = 'table',
}
interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigHttpRequest

const methodOptions = [
  { label: 'GET', value: 'GET' },
  { label: 'POST', value: 'POST' },
]

const contentTypeOptions = [
  { label: 'text/plain', value: 'text/plain' },
  { label: 'application/json', value: 'application/json' },
  { label: 'x-www-form-urlencoded', value: 'application/x-www-form-urlencoded' },
  { label: 'form-data', value: 'multipart/form-data' },
]

function onUpdateContentType(selected: string) {
  nodeConfig.content_type = selected
}

function onDeleteHeader(row: Workflow.NodeConfigHttpRequestParam) {
  const idx = nodeConfig.headers.findIndex(item => item.name === row.name)
  nodeConfig.headers.splice(idx, 1)
}

function onDeleteParam(row: Workflow.NodeConfigHttpRequestParam) {
  const idx = nodeConfig.params.findIndex(item => item.name === row.name)
  nodeConfig.params.splice(idx, 1)
}

function onDeleteFormDataBody(row: Workflow.NodeConfigHttpRequestParam) {
  const idx = nodeConfig.form_data_body.findIndex(item => item.name === row.name)
  nodeConfig.form_data_body.splice(idx, 1)
}

function onDeleteFormUrlEncodedBody(row: Workflow.NodeConfigHttpRequestParam) {
  const idx = nodeConfig.form_urlencoded_body.findIndex(item => item.name === row.name)
  nodeConfig.form_urlencoded_body.splice(idx, 1)
}

function onAddHeader() {
  nodeConfig.headers.push({
    name: '',
    value: '',
  })
}

function onAddParam() {
  nodeConfig.params.push({
    name: '',
    value: '',
  })
}

function onAddFormDataBody() {
  nodeConfig.form_data_body.push({
    name: '',
    value: '',
  })
}

function onAddFormUrlEncodedBody() {
  nodeConfig.form_urlencoded_body.push({
    name: '',
    value: '',
  })
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center">
        URL
      </div>
      <div>
        <NInput v-model:value="nodeConfig.url" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center">
        Method
      </div>
      <div>
        <NSelect
          :value="nodeConfig.method" :options="methodOptions"
          :on-update:value="(selected) => nodeConfig.method = selected"
        />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center">
        {{ t('workflow.timeoutSeconds') }}
      </div>
      <div>
        <NInputNumber v-model:value="nodeConfig.timeout" min="1" max="600" type="number" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center">
        {{ t('workflow.retryCount') }}
      </div>
      <div>
        <NInputNumber v-model:value="nodeConfig.retry_times" min="0" max="10" type="number" />
      </div>
    </div>
    <NCollapse :default-expanded-names="['default']" display-directive="show" class="mt-6">
      <NCollapseItem name="default" class="border border-gray-200 rounded-md m-2 p-2">
        <template #header>
          <div class="text-xl">
            {{ t('workflow.requestHeader') }}
          </div>
        </template>
        <div class="flex flex-col space-y-1">
          <NTable :single-line="true" size="small">
            <thead>
              <tr>
                <th>Key</th>
                <th>Value</th>
                <th>{{ t('common.operate') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(param, index) in nodeConfig.headers" :key="index">
                <td>
                  <NInput v-model:value="param.name" />
                </td>
                <td>
                  <NInput v-model:value="param.value" />
                </td>
                <td>
                  <SvgIcon icon="ep:remove" class="w-4 h-4 mr-2 cursor-pointer" @click="onDeleteHeader(param)" />
                </td>
              </tr>
            </tbody>
          </NTable>
          <NButton size="small" @click="onAddHeader">
            {{ t('workflow.addRequestHeader') }}
          </NButton>
        </div>
      </NCollapseItem>
    </NCollapse>
    <NCollapse :default-expanded-names="['default']" display-directive="show" class="my-6">
      <NCollapseItem name="default" class="border border-gray-200 rounded-md p-2">
        <template #header>
          <div class="text-xl">
            {{ t('workflow.requestParam') }}
          </div>
        </template>
        <div class="flex flex-col space-y-1">
          <NTable :single-line="true" size="small">
            <thead>
              <tr>
                <th>Key</th>
                <th>Value</th>
                <th>{{ t('common.operate') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(param, index) in nodeConfig.params" :key="index">
                <td>
                  <NInput v-model:value="param.name" />
                </td>
                <td>
                  <NInput v-model:value="param.value" />
                </td>
                <td>
                  <SvgIcon icon="ep:remove" class="w-4 h-4 mr-2 cursor-pointer" @click="onDeleteParam(param)" />
                </td>
              </tr>
            </tbody>
          </NTable>
          <NButton size="small" @click="onAddParam">
            {{ t('workflow.addParam') }}
          </NButton>
        </div>
      </NCollapseItem>
    </NCollapse>
    <div class="mt-2">
      <div class="text-xl mb-1 flex align-center">
        Content-Type
      </div>
      <div class="flex flex-col space-y-2 p-2">
        <NSelect
          :value="nodeConfig.content_type" :options="contentTypeOptions"
          :on-update:value="onUpdateContentType"
        />
        <div v-show="nodeConfig.content_type === 'application/json'">
          <JsonEditorVue
            v-model="nodeConfig.json_body" :main-menu-bar="false" :mode="Mode.text"
            style="width: 100%; height: 300px;"
          />
        </div>
        <div v-show="nodeConfig.content_type === 'text/plain'">
          <NInput
            v-model:value="nodeConfig.text_body" type="textarea" show-count
            :autosize="{ minRows: 5, maxRows: 10 }"
          />
        </div>
        <div v-show="nodeConfig.content_type === 'application/x-www-form-urlencoded'" class="flex flex-col space-y-1">
          <NTable :single-line="true" size="small">
            <thead>
              <tr>
                <th>Key</th>
                <th>Value</th>
                <th>{{ t('common.operate') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(param, index) in nodeConfig.form_urlencoded_body" :key="index">
                <td>
                  <NInput v-model:value="param.name" />
                </td>
                <td>
                  <NInput v-model:value="param.value" />
                </td>
                <td>
                  <SvgIcon
                    icon="ep:remove" class="w-4 h-4 mr-2 cursor-pointer"
                    @click="onDeleteFormUrlEncodedBody(param)"
                  />
                </td>
              </tr>
            </tbody>
          </NTable>
          <NButton size="small" @click="onAddFormUrlEncodedBody">
            {{ t('workflow.addParam') }}
          </NButton>
        </div>
        <div v-show="nodeConfig.content_type === 'multipart/form-data'" class="flex flex-col space-y-1">
          <NTable :single-line="true" size="small">
            <thead>
              <tr>
                <th>Key</th>
                <th>Value</th>
                <th>{{ t('common.operate') }}</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(param, index) in nodeConfig.form_data_body" :key="index">
                <td>
                  <NInput v-model:value="param.name" />
                </td>
                <td>
                  <NInput v-model:value="param.value" />
                </td>
                <td>
                  <SvgIcon icon="ep:remove" class="w-4 h-4 mr-2 cursor-pointer" @click="onDeleteFormDataBody(param)" />
                </td>
              </tr>
            </tbody>
          </NTable>
          <NButton size="small" @click="onAddFormDataBody">
            {{ t('workflow.addParam') }}
          </NButton>
        </div>
      </div>
      <div class="mt-6">
        <div class="text-xl mb-1 flex align-center items-center">
          {{ t('workflow.clearHtml') }}
          <NTooltip trigger="hover" placement="right" style="margin-left: 1.5rem;">
            <template #trigger>
              <SvgIcon icon="mingcute:question-line" class="text-base text-gray-500" />
            </template>
            {{ t('workflow.clearHtmlTip') }}
          </NTooltip>
        </div>
        <div>
          <NSwitch v-model:value="nodeConfig.clear_html" />
        </div>
      </div>
      <NCollapse :default-expanded-names="['default']" display-directive="show" class="my-6">
        <NCollapseItem name="default" class="border border-gray-200 rounded-md p-2">
          <template #header>
            <div class="text-xl">
              {{ t('workflow.outputVariable') }}
            </div>
          </template>
          <div class="flex flex-col space-y-2 p-2">
            <div>
              <NTag size="small" round>
                status_code
              </NTag>
              {{ t('workflow.httpStatusCode') }}
            </div>
            <div>
              <NTag size="small" round>
                output
              </NTag>
              {{ t('workflow.responseBody') }}
            </div>
          </div>
        </NCollapseItem>
      </NCollapse>
      <br>
      <br>
    </div>
  </div>
</template>
