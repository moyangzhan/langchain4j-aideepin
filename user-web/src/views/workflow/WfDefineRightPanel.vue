<script lang="ts" setup>
import { onMounted, ref, watch } from 'vue'
import { NInput } from 'naive-ui'
import { AgentNodeProperty, AnswerNodeProperty, ClassifierNodeProperty, DocumentExtractorNodeProperty, EndNodeProperty, FaqExtractorNodeProperty, GoogleNodeProperty, HttpRequestNodeProperty, HumanFeedbackNodeProperty, KeywordExtractorNodeProperty, KnowledgeRetrievalNodeProperty, MailSendNodeProperty, OpenAiImageNodeProperty, StartNodeProperty, SwticherNodeProperty, TemplateNodeProperty, TongyiwanxNodeProperty } from './components/nodes'
import { useWfStore } from '@/store'
import { t } from '@/locales'
import { SvgIcon } from '@/components/common'
import { getIconByComponentName, getIconClassByComponentName } from '@/utils/workflow-util'
import { emptyWorkflowNode } from '@/utils/functions'

interface Props {
  workflow: Workflow.WorkflowInfo
  uiWorkflow: Workflow.UIWorkflow
  hidePropertyPanel: boolean
  wfNode?: Workflow.WorkflowNode
}

const props = withDefaults(defineProps<Props>(), {
  hidePropertyPanel: false,
  wfNode: () => emptyWorkflowNode(),
})

const wfStore = useWfStore()
const nodeTitle = ref<string>(props.wfNode.title)

watch(
  () => props.wfNode,
  (newVal) => {
    if (newVal)
      nodeTitle.value = newVal.title
  },
  { immediate: true, deep: true },
)
watch(
  () => nodeTitle,
  (newVal) => {
    if (newVal)
      wfStore.updateWfNodeTitle(props.workflow.uuid, props.wfNode.uuid, newVal.value)
  },
  { immediate: true, deep: true },
)
const innerHeight = window.innerHeight < 800 ? 800 : window.innerHeight
onMounted(() => {
  console.log('props.wfNode.title', props.wfNode.title)
  nodeTitle.value = props.wfNode.title
})
</script>

<template>
  <div class="absolute right-0 top-20 bg-white rounded-lg shadow-xl">
    <!-- 右侧属性面板 -->
    <div v-if="!hidePropertyPanel && wfNode" class="px-3 pt-5 h-full" style="width:600px">
      <div class="w-full flex flex-col border-b divide-gray-400 pb-3 mb-5">
        <div class="text-3xl flex items-center h-10 mb-2">
          <SvgIcon
            class="mt-1 mr-2" :class="getIconClassByComponentName(wfNode.wfComponent.name)"
            :icon="getIconByComponentName(wfNode.wfComponent.name)"
          />
          <NInput
            v-model:value="nodeTitle" :placeholder="t('workflow.nodeNamePlaceholder')" class="h-8 border-gray-100"
            style="font-size: 1rem;line-height: 1.5rem;font-weight: 700;"
          />
        </div>
        <!-- <div class="mb-2">
                    <NInput v-model:value="selectedNode.data.remark" placeholder="该节点功能描述" />
                  </div> -->
        <div class="text-sm text-gray-500">
          {{ t('workflow.componentFunction') }}{{ wfNode.wfComponent.remark }}
        </div>
      </div>
      <div class="overflow-y-auto" :style="`height:${innerHeight - 250}px`">
        <StartNodeProperty
          v-if="wfNode.wfComponent.name === 'Start'" :key="wfNode.uuid" :workflow="workflow"
          :wf-node="wfNode"
        />
        <AnswerNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Answer'" :key="`answer_${wfNode.uuid}`"
          :workflow="workflow" :wf-node="wfNode"
        />
        <ClassifierNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Classifier'" :key="`classifier_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <SwticherNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Switcher'" :key="`switcher_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <TemplateNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Template'" :key="`template_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <KeywordExtractorNodeProperty
          v-else-if="wfNode.wfComponent.name === 'KeywordExtractor'"
          :key="`keyword_${wfNode.uuid}`" :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <DocumentExtractorNodeProperty
          v-else-if="wfNode.wfComponent.name === 'DocumentExtractor'"
          :key="`document_${wfNode.uuid}`" :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <FaqExtractorNodeProperty
          v-else-if="wfNode.wfComponent.name === 'FaqExtractor'" :key="`faq_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <KnowledgeRetrievalNodeProperty
          v-else-if="wfNode.wfComponent.name === 'KnowledgeRetrieval'"
          :key="`knowledge_${wfNode.uuid}`" :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <OpenAiImageNodeProperty
          v-else-if="wfNode.wfComponent.name === 'OpenAiImage'" :key="`openaiimage_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <TongyiwanxNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Tongyiwanx'" :key="`tongyiwanx_${wfNode.uuid}`"
          :workflow="workflow" :wf-node="wfNode"
        />
        <GoogleNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Google'" :key="`google_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <HumanFeedbackNodeProperty
          v-else-if="wfNode.wfComponent.name === 'HumanFeedback'" :key="`feedback_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <MailSendNodeProperty
          v-else-if="wfNode.wfComponent.name === 'MailSend'" :key="`mailsend_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <HttpRequestNodeProperty
          v-else-if="wfNode.wfComponent.name === 'HttpRequest'" :key="`httprequest_${wfNode.uuid}`"
          :workflow="workflow" :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
        <AgentNodeProperty
          v-else-if="wfNode.wfComponent.name === 'Agent'" :key="`agent_${wfNode.uuid}`"
          :workflow="workflow" :wf-node="wfNode"
        />
        <EndNodeProperty
          v-else-if="wfNode.wfComponent.name === 'End'" :key="`end_${wfNode.uuid}`" :workflow="workflow"
          :ui-workflow="uiWorkflow" :wf-node="wfNode"
        />
      </div>
    </div>
    <!-- 右侧属性面板 end -->
  </div>
</template>
