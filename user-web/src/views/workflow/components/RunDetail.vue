<script lang="ts" setup>
import { nextTick, onUnmounted, reactive, ref, watch } from 'vue'
import { NButton, NInput, NInputNumber, NP, NSwitch, NTab, NTabPane, NTabs, NText, NUpload, NUploadDragger, useMessage } from 'naive-ui'
import type { UploadFileInfo, UploadInst } from 'naive-ui'
import RuntimeNodes from './RuntimeNodes.vue'
import { useAuthStore, useWfStore } from '@/store'
import { SvgIcon } from '@/components/common'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
}
interface TabObj {
  name: string
  tab: string
  defaultTab: string
}
interface Emit {
  (e: 'runDone'): void
  (e: 'runError', errorMsg: string): void
}
const props = defineProps<Props>()
const emit = defineEmits<Emit>()
const headers = { Authorization: '' }
const wfStore = useWfStore()
const authStore = useAuthStore()
const token = ref<string>(authStore.token)
const ms = useMessage()
const submitting = ref<boolean>(wfStore.submitting)
const startNode = wfStore.getStartNode(props.workflow.uuid)
const wfRuntimeUuid = ref<string>('')
const runtimeNodes = reactive<Workflow.WfRuntimeNode[]>([])
const runtimeErrorMsg = ref<string>('')
const userInputs = ref<Workflow.UserInput[]>(startNode?.inputConfig.user_inputs.map((input) => {
  return {
    uuid: input.uuid,
    name: input.name,
    content: {
      title: input.title,
      value: null,
      type: input.type,
    },
    required: input.required,
  }
}) || [])
const errorMsg = ref<string>('')
const currWfUuid = props.workflow.uuid
console.log('instance list currWfUuid', currWfUuid)
const showCurrentExecution = ref<boolean>(false)
const tabObj = ref<TabObj>({ name: 'runtimes', defaultTab: t('workflow.flowRunDetail'), tab: `${t('workflow.flowRunDetail')} ↓` })
const fileListLength = ref(0)
const uploadRef = ref<UploadInst | null>(null)
const uploadedFileUuids = ref<string[]>([])
const humanFeedback = ref<boolean>(false)
const humanFeedbackTip = ref<string>('')
const humanFeedbackContent = ref<string>('')
let controller = new AbortController()

async function uploadBeforeRun() {
  uploadedFileUuids.value = []
  if (uploadRef.value && Array.isArray(uploadRef.value) && uploadRef.value.length > 0)
    uploadRef.value[0]?.submit()
  else if (uploadRef.value)
    uploadRef.value?.submit()
}

async function resetInputs() {
  if (uploadRef.value && Array.isArray(uploadRef.value) && uploadRef.value.length > 0) {
    uploadRef.value.forEach((item) => {
      item.clear()
    })
  } else if (uploadRef.value) {
    uploadRef.value?.clear()
  }
  uploadedFileUuids.value = []

  userInputs.value.forEach((input) => {
    input.content.value = null
  })
}

async function run() {
  if (!authStore.checkLoginOrShow())
    return

  if (submitting.value)
    return

  for (const input of userInputs.value) {
    if (input.required && input.content.type === 4 && input.content.value === null && fileListLength.value === 0) {
      ms.warning(t('workflow.pleaseUploadFile'))
      return
    }
  }

  if (fileListLength.value > 0 && uploadedFileUuids.value.length !== fileListLength.value) {
    console.log('先执行文件上传操作')
    uploadBeforeRun()
    return
  } else {
    const fileInput = userInputs.value.find(input => input.content.type === 4 && input.content.value === null)
    if (fileInput)
      fileInput.content.value = uploadedFileUuids.value
  }

  if (userInputs.value.some(input => input.required && input.content.value === null)) {
    console.log('请输入所有必填参数')
    ms.warning(t('workflow.pleaseInputAllRequired'))
    return
  }

  submitting.value = true
  showCurrentExecution.value = true
  tabObj.value.tab = showCurrentExecution.value ? `${tabObj.value.defaultTab} ↓` : `${tabObj.value.defaultTab} ↑`

  controller = new AbortController()
  try {
    wfRuntimeUuid.value = ''
    const nodeUuidToRuntimeNodeUuid = new Map<string, string>()
    runtimeNodes.splice(0, runtimeNodes.length)
    await api.workflowRun({
      options: {
        uuid: currWfUuid,
        inputs: userInputs.value,
      },
      signal: controller.signal,
      startCallback: (wfRuntimeJson) => {
        if (!wfRuntimeJson) {
          ms.error(t('workflow.startFailed'))
          return
        }
        const wfRuntime = JSON.parse(wfRuntimeJson) as Workflow.WorkflowRuntime
        wfRuntime.input = {}
        userInputs.value.forEach((item) => {
          wfRuntime.input[item.name] = { ...item.content }
        })
        wfRuntimeUuid.value = wfRuntime.uuid
        wfStore.appendWfRuntimes(
          currWfUuid,
          [wfRuntime],
        )
      },
      thinkingDataReceived: (chunk) => {
        // 处理思考数据
        console.log('Thinking data received:', chunk)
      },
      messageReceived: (chunk, event) => {
        const eventName = event || ''
        try {
          if (eventName.includes('[NODE_RUN_')) {
            const nodeUuid = eventName.replace('[NODE_RUN_', '').replace(']', '')
            console.log(`${nodeUuid}开始运行`)
            const runtimeNode = JSON.parse(chunk) as Workflow.WfRuntimeNode
            nodeUuidToRuntimeNodeUuid.set(nodeUuid, runtimeNode.uuid)
            wfStore.appendRuntimeNode(
              wfRuntimeUuid.value,
              runtimeNode,
            )
            runtimeNodes.push(runtimeNode)
          } else if (eventName.includes('[NODE_CHUNK_')) {
            const nodeUuid = eventName.replace('[NODE_CHUNK_', '').replace(']', '')
            const runtimeNodeUuid = nodeUuidToRuntimeNodeUuid.get(nodeUuid) || ''
            wfStore.appendChunkToRuntimeNode(
              wfRuntimeUuid.value,
              runtimeNodeUuid,
              chunk,
            )
          } else if (eventName.includes('[NODE_INPUT_')) {
            const nodeUuid = eventName.replace('[NODE_INPUT_', '').replace(']', '')
            const runtimeNodeUuid = nodeUuidToRuntimeNodeUuid.get(nodeUuid) || ''
            wfStore.appendInputToRuntimeNode(
              wfRuntimeUuid.value,
              runtimeNodeUuid,
              chunk,
            )
          } else if (eventName.includes('[NODE_OUTPUT_')) {
            const nodeUuid = eventName.replace('[NODE_OUTPUT_', '').replace(']', '')
            const runtimeNodeUuid = nodeUuidToRuntimeNodeUuid.get(nodeUuid) || ''
            wfStore.appendOutputToRuntimeNode(
              wfRuntimeUuid.value,
              runtimeNodeUuid,
              chunk,
            )
          } else if (eventName.includes('[NODE_WAIT_FEEDBACK_BY_')) {
            humanFeedback.value = true
            humanFeedbackTip.value = chunk || ''
            ms.info(humanFeedbackTip.value)
          }
        } catch (error) {
          console.error(error)
        }
      },
      doneCallback: (chunk) => {
        nextTick(() => {
          submitting.value = false
          resetInputs()
          wfStore.updateSuccess(currWfUuid, wfRuntimeUuid.value, chunk)
          runtimeErrorMsg.value = ''
          ms.success(t('workflow.runSuccess'))
          emit('runDone')
        })
      },
      errorCallback: (error) => {
        submitting.value = false
        resetInputs()
        ms.error(`${t('common.systemTip')}${error}`)
        wfStore.updateErrorMsg(currWfUuid, wfRuntimeUuid.value, error)
        runtimeErrorMsg.value = error || ''
        emit('runError', error)
      },
    })
  } catch (error: any) {
    const errorMessage = error?.message ?? t('common.wrong')
    ms.error(errorMessage)
    submitting.value = false
  }
}

async function resume() {
  submitting.value = true
  try {
    await api.workflowRuntimeResume({
      runtimeUuid: wfRuntimeUuid.value,
      feedbackContent: humanFeedbackContent.value,
    },
    )
  } catch (e) {
    ms.error(`${t('common.systemTip')}${e}`)
  } finally {
    humanFeedback.value = false
    humanFeedbackTip.value = ''
    humanFeedbackContent.value = ''
  }
}

function onUploadChange(options: { fileList: UploadFileInfo[] }) {
  console.log('onUploadChange', options)
}

function handleFileListChange(fileList: UploadFileInfo[]) {
  console.log('handleFileListChange', fileList)
  fileListLength.value = fileList.length
  if (uploadedFileUuids.value.length === fileListLength.value)
    run()
}

function onUploadFinish({ file, event }: { file: UploadFileInfo; event?: ProgressEvent }) {
  console.log('onUploadFinish', file, event)
  const res = JSON.parse((event?.target as XMLHttpRequest).response)
  if (res.success) {
    console.log('uploaded file data:', res.data)
    uploadedFileUuids.value.push(res.data.uuid)
  } else {
    console.log(`onUploadFinish err:${res.data}`)
  }
  return file
}

function handleStop() {
  if (wfStore.wfUuidToWfRuntimeLoading.get(currWfUuid))
    controller.abort()

  submitting.value = false
}

function handleClick() {
  showCurrentExecution.value = !showCurrentExecution.value
  tabObj.value.tab = showCurrentExecution.value ? `${tabObj.value.defaultTab} ↓` : `${tabObj.value.defaultTab} ↑`
}

watch(
  () => token,
  () => {
    if (token.value)
      headers.Authorization = token.value
  },
  { immediate: true },
)

onUnmounted(() => {
  if (wfStore.wfUuidToWfRuntimeLoading.get(currWfUuid))
    controller.abort()
})
</script>

<template>
  <div class="w-full max-w-screen-xl m-auto z-10">
    <NTabs type="line" justify-content="space-evenly" animated default-value="runtimes">
      <NTab name="runtimes" @click="handleClick">
        {{ tabObj.tab }}
      </NTab>
    </NTabs>
    <NTabs type="line" justify-content="space-evenly" animated>
      <NTabPane name="runtimes" display-directive="show" :tab-props="{ style: 'display:none' }">
        <transition name="collapse">
          <div v-show="showCurrentExecution" class="max-h-[500px] overflow-y-auto mb-2">
            <RuntimeNodes :nodes="runtimeNodes" :workflow="workflow" :error-msg="runtimeErrorMsg" />
            <div class="sticky bottom-0 left-0 flex justify-center">
              <NButton v-show="submitting" size="tiny" @click="handleStop">
                <template #icon>
                  <SvgIcon icon="ri:stop-circle-line" />
                </template>
                {{ t('common.stopRequest') }}
              </NButton>
            </div>
          </div>
        </transition>
      </NTabPane>
    </NTabs>
    <div v-if="errorMsg">
      {{ errorMsg }}
    </div>
    <div class="flex flex-col items-center justify-between space-y-2 max-h-[300px] overflow-y-auto">
      <template v-if="!humanFeedback">
        <div v-for="(userInput, idx) in userInputs" :key="`${idx}_${userInput.name}`" class="w-full flex">
          <div class="min-w-24">
            {{ userInput.content.title }}
          </div>
          <!-- 文本 -->
          <NInput
            v-if="userInput.content.type === 1" v-model:value="userInput.content.value" type="textarea"
            :autosize="{ minRows: 1, maxRows: 5 }"
          />
          <!-- 数字 -->
          <NInputNumber v-if="userInput.content.type === 2" v-model:value="userInput.content.value" />
          <!-- 下拉列表 -->
          <div v-if="userInput.content.type === 3" />
          <!-- 文件列表 -->
          <NUpload
            v-if="userInput.content.type === 4" ref="uploadRef" multiple directory-dnd action="/api/file/upload"
            :default-upload="false"
            :max="startNode?.inputConfig.user_inputs.find(item => item.uuid === userInput.uuid)?.limit || 10"
            :headers="headers" @update:file-list="handleFileListChange" @finish="onUploadFinish"
            @change="onUploadChange"
          >
            <NUploadDragger>
              <NText style="font-size: 16px">
                {{ t('workflow.clickOrDragToUpload') }}
              </NText>
              <NP depth="2" style="margin: 4px 0 0 0">
                {{ t('workflow.fileFormatSizeLimit') }}
              </NP>
            </NUploadDragger>
          </NUpload>
          <!-- 布尔值 -->
          <NSwitch v-if="userInput.content.type === 5" v-model:value="userInput.content.value" />
        </div>
        <div class="w-full flex justify-end">
          <NButton type="primary" :disabled="submitting" :loading="submitting" @click="run">
            {{ t('common.submit') }}
          </NButton>
        </div>
      </template>
      <!-- 流程执行过程中用户的输入 -->
      <template v-if="humanFeedback">
        <div class="flex flex-col p-2 w-full space-y-2">
          <div class="flex bg-gray-100 px-2 py-1 rounded-md">
            <div class="text-base text-red-500">
              {{ t('workflow.flowPausedWaitingInput') }}
            </div>
          </div>
          <div class="flex flex-col w-full">
            <div v-if="humanFeedbackTip" class="text-sm leading-8">
              {{ t('workflow.inputTip') }}{{ humanFeedbackTip }}
            </div>
            <NInput v-model:value="humanFeedbackContent" type="textarea" :autosize="{ minRows: 2, maxRows: 5 }" />
          </div>
          <div class="flex justify-end">
            <NButton type="primary" @click="resume">
              {{ t('common.submit') }}
            </NButton>
          </div>
        </div>
      </template>
    </div>
  </div>
</template>
