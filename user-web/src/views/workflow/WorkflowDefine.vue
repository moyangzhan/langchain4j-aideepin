<script setup lang='ts'>
import { computed, nextTick, onMounted, onUnmounted, reactive, ref } from 'vue'
import { NButton, NLayout, NLayoutContent, NLayoutSider, NModal, NTooltip, useMessage } from 'naive-ui'
import type { Edge, Node, NodeChange } from '@vue-flow/core'
import { VueFlow, useVueFlow } from '@vue-flow/core'
import { Background } from '@vue-flow/background'
import { AnswerNode, ClassifierNode, DocumentExtractorNode, EndNode, FaqExtractorNode, GoogleNode, HttpRequestNode, HumanFeedbackNode, KeywordExtractorNode, KnowledgeRetrievalNode, MailSendNode, OpenAiImageNode, SpecialNode, StartNode, SwitcherNode, TemplateNode, TongyiwanxNode } from './components/nodes'
import SpecialEdge from './components/edges/SpecialEdge.vue'
import CustomEdge from './components/edges/CustomEdge.vue'
import CustomEdge2 from './components/edges/CustomEdge2.vue'
import RunDetail from '@/views/workflow/components/RunDetail.vue'
import WfDefineRightPanel from '@/views/workflow/WfDefineRightPanel.vue'
import WfDefineSidebar from '@/views/workflow/WfDefineSiderbar.vue'
import { emptyWorkflowInfo } from '@/utils/functions'
import { createNewEdge, createNewNode } from '@/utils/workflow-util'
import { useUserStore, useWfStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
}
const props = withDefaults(defineProps<Props>(), {
  workflow: () => emptyWorkflowInfo(),
})

const showRunModal = ref<boolean>(false)
const ms = useMessage()
const submitting = ref<boolean>(false)
const hidePropertyPanel = ref<boolean>(true)
const selectedWfNode = ref<Workflow.WorkflowNode>()
const wfStore = useWfStore()
const userStore = useUserStore()

const saveDisabledTip = computed(() => {
  if (userStore.userInfo.uuid !== props.workflow.userUuid)
    return t('workflow.onlyCreatorCanSave')
  if (submitting.value)
    return t('workflow.saving')
  return ''
})

const { onInit, fitView, onConnect, onNodeClick, onEdgeClick, onNodesChange, onEdgesChange, onNodeDragStop, addSelectedNodes, project } = useVueFlow()

const uw = wfStore.wfUuidToUIWorkflow.get(props.workflow.uuid) || { nodes: [] as Array<Node>, edges: [] as Array<Edge> }
wfStore.wfUuidToUIWorkflow.set(props.workflow.uuid, uw)
const uiWorkflow = reactive(uw)

function renderGraph() {
  if (uiWorkflow.nodes.length > 0)
    return

  console.log('renderGraph')
  const initX = 10
  const initY = 50
  const wfNodes = props.workflow.nodes
  const wfEdges = props.workflow.edges
  for (let i = 0; i < wfNodes.length; i++) {
    const node = wfNodes[i]
    const px = node.positionX ? node.positionX : initX + 230 * i
    const py = node.positionY ? node.positionY : initY
    const newNode = {
      id: node.uuid,
      type: node.wfComponent.name.toLowerCase(),
      data: node,
      position: { x: px, y: py },
    }
    uiWorkflow.nodes.push(newNode)
  }
  for (let i = 0; i < wfEdges.length; i++) {
    const wfEdge = wfEdges[i]
    const sourceNode = props.workflow.nodes.find((item: Workflow.WorkflowNode) => item.uuid === wfEdge.sourceNodeUuid)
    if (!sourceNode) {
      console.error('sourceNode not found', wfEdge.sourceNodeUuid)
      continue
    }
    const newEdge = {
      id: wfEdge.uuid,
      source: wfEdge.sourceNodeUuid,
      target: wfEdge.targetNodeUuid,
      sourceHandle: wfEdge.sourceHandle,
      type: 'special',
      animated: true,
      data: wfEdge,
    }
    uiWorkflow.edges.push(newEdge)
  }
}

onNodesChange((changes: NodeChange[]) => {
  let nodeUnSelected = false
  for (const change of changes) {
    if ('selected' in change) {
      if (!change.selected && selectedWfNode.value?.uuid === change.id) {
        nodeUnSelected = true
        console.log('node selected', change.id)
      }
    }
    // else if ('dragging' in change) {
    //   //停止拖动时才计算是否要打开属性面板
    //   if (change.dragging) {
    //     const snode = props.workflow.nodes.find((item: Workflow.WorkflowNode) => item.uuid === change.id)
    //     if (snode) {
    //       snode.positionX = change.position.x
    //       snode.positionY = change.position.y
    //     }
    //   }
    // }
  }
  if (nodeUnSelected)
    hidePropertyPanel.value = true
})

onEdgesChange((changes) => {
  // changes are arrays of type `EdgeChange`
  console.log(changes)
})

/**
 * onConnect is called when a new connection is created.
 *
 * You can add additional properties to your new edge (like a type or label) or block the creation altogether by not calling `addEdges`
 */
onConnect((connection) => {
  console.log('onConnect', connection)
  createNewEdge({
    workflow: props.workflow,
    uiWorkflow,
    source: connection.source,
    sourceHandle: connection.sourceHandle ? connection.sourceHandle : '',
    target: connection.target,
  })
})

// any event that is emitted from the `<VueFlow />` component can be listened to using the `onEventName` method
onInit(() => {
  console.log('onInit')
  // `instance` is the same type as the return of `useVueFlow` (VueFlowStore)
  nextTick(() => {
    fitView()
  })
})

function onDragOver(event: DragEvent) {
  event.preventDefault()
  if (event.dataTransfer)
    event.dataTransfer.dropEffect = 'move'
}

const wrapper = ref()

// onConnect(addEdges)

function onDrop(event: DragEvent) {
  const comName = event.dataTransfer?.getData('application/vueflow') as string
  const component = wfStore.getWfComponent(comName)
  if (!component) {
    console.error('component not found', comName)
    return
  }
  if (comName === 'Start') {
    ms.warning(t('workflow.startNodeOnlyOne'))
    return
  }

  const flowbounds = wrapper.value.$el.getBoundingClientRect()

  const position = project({
    x: event.clientX - flowbounds.left,
    y: event.clientY - flowbounds.top,
  })

  createNewNode(props.workflow, uiWorkflow, component, position)

  addSelectedNodes([uiWorkflow.nodes[uiWorkflow.nodes.length - 1]])
}

/**
 * onNodeDragStop is called when a node is done being dragged
 *
 * Node drag events provide you with:
 * 1. the event object
 * 2. the nodes array (if multiple nodes are dragged)
 * 3. the node that initiated the drag
 * 4. any intersections with other nodes
 */
onNodeDragStop(({ event, nodes, node }) => {
  // selectedWfNode.value = node.data
  console.log('Node drag stop:', nodes, node, event)
  if (node) {
    node.data.positionX = node.position.x
    node.data.positionY = node.position.y
  }
  // hidePropertyPanel.value = false
})

// Node click event handler
onNodeClick(({ event, node }) => {
  console.log('Node clicked:', node, event)
  if (node.selected) {
    hidePropertyPanel.value = false
    selectedWfNode.value = node.data
  }
})

// Edge click event handler
onEdgeClick(({ event, edge }) => {
  console.log('Edge clicked:', edge, event)
  if (selectedWfNode.value)
    hidePropertyPanel.value = false
})

function onRun() {
  showRunModal.value = true
}

async function onSave() {
  if (submitting.value)
    return

  submitting.value = true
  try {
    const { data: updatedWorkflow } = await api.workflowUpdate(props.workflow)
    ms.success(t('common.saveSuccessTip'))
    // 只需要更新新增节点和边的id
    wfStore.updateNodesAndEdgesId(props.workflow.uuid, updatedWorkflow)
  } catch (e) {
    console.log(e)
  } finally {
    submitting.value = false
  }
}

onMounted(() => {
  console.log('workflow define onMounted', this)
  nextTick(() => {
    if (wfStore.wfComponents.length === 0) {
      setTimeout(() => {
        renderGraph()
      }, 2000)
    } else {
      renderGraph()
    }
  })
})

onUnmounted(() => {
  console.log('workflow define onUnMounted', this)
})
</script>

<template>
  <div class="chat-box flex flex-col w-full h-full">
    <main class="flex-1 overflow-hidden">
      <div ref="scrollRef" class="h-full overflow-hidden overflow-y-auto">
        <div class="flex h-full">
          <div class="flex-1 dndflow" @drop="onDrop">
            <NLayout has-sider class="h-full">
              <NLayoutSider
                collapse-mode="transform" show-trigger="bar" :collapsed-width="12" :width="240"
                :show-collapsed-content="false" content-style="padding: 12px;" bordered
              >
                <!-- 节点菜单 -->
                <WfDefineSidebar />
              </NLayoutSider>
              <NLayoutContent class="h-full" style="background:#f5f5f5">
                <!-- 流程图设计器 begin -->
                <VueFlow
                  ref="wrapper" :nodes="uiWorkflow.nodes" :edges="uiWorkflow.edges" fit-view-on-init
                  @dragover="onDragOver"
                >
                  <Background />
                  <!-- bind your custom node type to a component by using slots, slot names are always `node-<type>` -->
                  <template #node-start="nodeProps">
                    <StartNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-end="nodeProps">
                    <EndNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-answer="nodeProps">
                    <AnswerNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-classifier="nodeProps">
                    <ClassifierNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-documentextractor="nodeProps">
                    <DocumentExtractorNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-openaiimage="nodeProps">
                    <OpenAiImageNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-tongyiwanx="nodeProps">
                    <TongyiwanxNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-knowledgeretrieval="nodeProps">
                    <KnowledgeRetrievalNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-keywordextractor="nodeProps">
                    <KeywordExtractorNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-google="nodeProps">
                    <GoogleNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-switcher="nodeProps">
                    <SwitcherNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-template="nodeProps">
                    <TemplateNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-faqextractor="nodeProps">
                    <FaqExtractorNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-humanfeedback="nodeProps">
                    <HumanFeedbackNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-mailsend="nodeProps">
                    <MailSendNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-httprequest="nodeProps">
                    <HttpRequestNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <template #node-special="nodeProps">
                    <SpecialNode v-bind="nodeProps" :workflow="workflow" />
                  </template>
                  <!-- bind your custom edge type to a component by using slots, slot names are always `edge-<type>` -->
                  <template #edge-special="edgeProps">
                    <SpecialEdge v-bind="edgeProps" />
                  </template>
                  <template #edge-custom="cprops">
                    <CustomEdge v-bind="cprops" />
                  </template>
                  <template #edge-custom2="ccprops">
                    <CustomEdge2 v-bind="ccprops" />
                  </template>
                </VueFlow>
                <div class="absolute right-5 top-3 flex items-center">
                  <NButton
                    :disabled="submitting" text-color="black" color="white" style="margin-right:1.5rem"
                    class="shadow-lg" @click="onRun"
                  >
                    {{ t('workflow.run') }}
                  </NButton>
                  <NTooltip v-if="saveDisabledTip" :disabled="!saveDisabledTip">
                    <template #trigger>
                      <NButton
                        :disabled="!!saveDisabledTip" :loading="submitting"
                        type="info" class="shadow-lg" @click="onSave"
                      >
                        {{ t('workflow.save') }}
                      </NButton>
                    </template>
                    {{ saveDisabledTip }}
                  </NTooltip>
                  <NButton
                    v-else :loading="submitting"
                    type="info" class="shadow-lg" @click="onSave"
                  >
                    {{ t('workflow.save') }}
                  </NButton>
                </div>
                <WfDefineRightPanel
                  :workflow="workflow" :ui-workflow="uiWorkflow"
                  :hide-property-panel="hidePropertyPanel" :wf-node="selectedWfNode"
                />
                <!-- 流程图设计器 end -->
              </NLayoutContent>
            </NLayout>
          </div>
          <NModal
            v-model:show="showRunModal" :mask-closable="false" :auto-focus="false" preset="card"
            style="width: 95%; max-width: 800px"
          >
            <RunDetail :workflow="workflow" />
          </NModal>
        </div>
      </div>
    </main>
  </div>
</template>

<style>
@import '@vue-flow/core/dist/style.css';
@import '@vue-flow/core/dist/theme-default.css';

/* @import '~@vue-flow/controls@latest/dist/style.css';
@import '~@vue-flow/minimap@latest/dist/style.css';
@import '~@vue-flow/node-resizer@latest/dist/style.css'; */
.dndflow {
  flex-direction: column;
  display: flex;
  height: 100%;
}

.dndflow aside {
  border-right: 1px solid #eee;
  padding: 15px 10px;
  font-size: 12px;
  background: #fcfcfc;
}

.dndflow aside>* {
  margin-bottom: 10px;
}

.vue-flow__node {
  border: 1px solid #eee;
  box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1);
  padding: 10px;
  border-radius: 10px;
  background: #FFF;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: center;
  gap: 10px;
  width: 220px;
}

.vue-flow__node.selected {
  border: 1px solid #2563eb;
  padding: 10px;
  border-radius: 10px;
}

.vue-flow__node.selected .vue-flow__handle {
  background: #2563eb;
}

.vue-flow__edge.selected .vue-flow__edge-path {
  stroke: #2563eb;
  stroke-width: 1.5;
}

.vue-flow__handle {
  background: #555;
  height: 16px;
  width: 8px;
  border-radius: 4px
}

.vue-flow__node .header {
  height: 45px;
  line-height: 45px;
  margin-bottom: 10px;
  text-align: center;
  font-weight: 600;
}

/* h-10 leading-10 bg-gray-100 mb-1 text-center */
.vue-flow__node .content_line {
  height: 40px;
  line-height: 40px;
  background: #9696961a;
  margin-bottom: 10px;
  text-align: center;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.right_side .n-scrollbar {
  min-width: 400px !important;
}

.dndflow .n-collapse .n-collapse-item .n-collapse-item__header {
  padding-top: 5px;
  padding-bottom: 5px;
}

.dndflow .n-collapse .n-collapse-item .n-collapse-item__content-wrapper .n-collapse-item__content-inner {
  padding-top: 0px;
}

.dndflow .n-collapse-item-arrow {
  margin-left: 0.5rem;
}

@media screen and (min-width: 768px) {
  .dndflow {
    flex-direction: row;
  }

}
</style>
