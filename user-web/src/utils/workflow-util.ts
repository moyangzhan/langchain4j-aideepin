import { v4 as uuidv4 } from 'uuid'
import { emptyWorkflowNode } from './functions'
import { useAppStore } from '@/store'
import { t } from '@/locales'

export function createNewNode(
  workflow: Workflow.WorkflowInfo,
  uiWorkflow: Workflow.UIWorkflow,
  component: Workflow.WorkflowComponent,
  position: { x: number; y: number },
) {
  const newWfNode = emptyWorkflowNode()
  newWfNode.uuid = uuidv4().replace(/-/g, '')
  newWfNode.title = t(`workflow.componentTitle.${component.name}`)
  newWfNode.workflowId = workflow.id
  newWfNode.workflowUuid = workflow.uuid
  newWfNode.wfComponent = component
  newWfNode.workflowComponentId = component.id
  newWfNode.inputConfig = { user_inputs: [], ref_inputs: [] }
  newWfNode.nodeConfig = {}
  newWfNode.outputConfig = {}
  newWfNode.positionX = position.x
  newWfNode.positionY = position.y
  if (component.name === 'Classifier')
    createClassifierNode(newWfNode)
  else if (component.name === 'Answer')
    createAnswer(newWfNode)
  else if (component.name === 'Switcher')
    createSwitcherNode(workflow, newWfNode)
  else if (component.name === 'KeywordExtractor')
    createKeywordExtractor(newWfNode)
  else if (component.name === 'FaqExtractor')
    createFaqExtractor(newWfNode)
  else if (component.name === 'KnowledgeRetrieval')
    createKnowledgeRetrieval(newWfNode)
  else if (component.name === 'OpenAiImage')
    createOpenAiImage(newWfNode)
  else if (component.name === 'Tongyiwanx')
    createTongyiwanx(newWfNode)
  else if (component.name === 'Google')
    createGoogle(newWfNode)
  else if (component.name === 'HumanFeedback')
    createHumanFeedback(newWfNode)
  else if (component.name === 'MailSend')
    createMailSend(newWfNode)
  else if (component.name === 'HttpRequest')
    createHttpRequest(newWfNode)
  else if (component.name === 'Agent')
    createAgent(newWfNode)

  workflow.nodes.push(newWfNode)
  uiWorkflow.nodes.push(wfNodeToUiNode(newWfNode))
}

export function createNewEdge(params: {
  workflow: Workflow.WorkflowInfo
  uiWorkflow: Workflow.UIWorkflow
  source: string
  sourceHandle: string
  target: string
},
) {
  const wfEdge = {
    id: '',
    uuid: uuidv4().replace(/-/g, ''),
    workflowId: params.workflow.id,
    sourceNodeUuid: params.source,
    sourceHandle: params.sourceHandle,
    targetNodeUuid: params.target,
    workflowUuid: params.workflow.uuid,
  }
  params.workflow.edges.push(wfEdge)
  if (params.target) {
    const uiEdge = {
      id: wfEdge.uuid,
      source: wfEdge.sourceNodeUuid,
      target: wfEdge.targetNodeUuid,
      type: 'special',
      animated: true,
      sourceHandle: params.sourceHandle ? params.sourceHandle : undefined,
      data: wfEdge,
    }
    params.uiWorkflow.edges.push(uiEdge)
  }
  // 判断源节点是否【条件分支】或【内容分类】
  params.workflow.nodes.forEach((node) => {
    if (node.uuid === params.source) {
      if (node.wfComponent.name === 'Switcher') {
        if (wfEdge.sourceHandle === 'default_handle') {
          (node.nodeConfig as Workflow.NodeConfigSwitcher).default_target_node_uuid = wfEdge.targetNodeUuid
        } else {
          (node.nodeConfig as Workflow.NodeConfigSwitcher).cases.forEach((item) => {
            if (item.uuid === wfEdge.sourceHandle)
              item.target_node_uuid = wfEdge.targetNodeUuid
          })
        }
      } else if (node.wfComponent.name === 'Classifier') {
        (node.nodeConfig as Workflow.NodeConfigClassifier).categories.forEach((item) => {
          if (item.category_uuid === wfEdge.sourceHandle)
            item.target_node_uuid = wfEdge.targetNodeUuid
        })
      }
    }
  })
}

export function updateEdgeBySourceHandle(params:
{
  workflow: Workflow.WorkflowInfo
  uiWorkflow: Workflow.UIWorkflow
  source: string
  sourceHandle: string
  target: string
},
) {
  const wfEdge = params.workflow.edges.find(item => item.sourceHandle === params.sourceHandle)
  if (!wfEdge) {
    console.warn('no edge found for sourceHandle')
    return
  }
  wfEdge.targetNodeUuid = params.target
  const idx = params.uiWorkflow.edges.findIndex(item => item.source === params.source && item.sourceHandle === params.sourceHandle)
  if (idx > -1)
    params.uiWorkflow.edges.splice(idx, 1)

  const uiEdge = {
    id: wfEdge.uuid,
    source: wfEdge.sourceNodeUuid,
    target: wfEdge.targetNodeUuid,
    animated: true,
    sourceHandle: params.sourceHandle,
  }
  params.uiWorkflow.edges.push(uiEdge)
}

export function deleteEdgesBySource(
  workflow: Workflow.WorkflowInfo,
  uiWorkflow: Workflow.UIWorkflow,
  source: string,
) {
  const edges = workflow.edges.filter(edge => edge.sourceNodeUuid === source)
  edges.forEach((element) => {
    const edgeIndex = workflow.edges.findIndex((edge) => {
      const hit = edge.uuid === element.uuid
      if (hit)
        workflow.deleteEdges.push(edge.uuid)

      return hit
    })
    if (edgeIndex !== -1)
      workflow.edges.splice(edgeIndex, 1)

    const uiEdgeIndex = uiWorkflow.edges.findIndex(edge => edge.uuid === element.uuid)
    if (uiEdgeIndex !== -1)
      uiWorkflow.edges.splice(edgeIndex, 1)
  })
}

export function deleteEdgesBySourceHandle(
  workflow: Workflow.WorkflowInfo,
  uiWorkflow: Workflow.UIWorkflow,
  source: string,
  sourceHandle: string,
) {
  const edgeIndex = workflow.edges.findIndex((edge) => {
    const hit = edge.sourceNodeUuid === source && edge.sourceHandle === sourceHandle
    if (hit)
      workflow.deleteEdges.push(edge.uuid)

    return hit
  })
  if (edgeIndex !== -1)
    workflow.edges.splice(edgeIndex, 1)

  const uiEdgeIndex = uiWorkflow.edges.findIndex(edge => edge.sourceNodeUuid === source && edge.sourceHandle === sourceHandle)
  if (uiEdgeIndex !== -1)
    uiWorkflow.edges.splice(edgeIndex, 1)
}

function wfNodeToUiNode(node: Workflow.WorkflowNode) {
  const newNode = {
    id: node.uuid,
    type: node.wfComponent.name.toLowerCase(),
    data: node,
    position: {
      x: node.positionX,
      y: node.positionY,
    },
  }
  return newNode
}

function createSwitcherNode(workflow: Workflow.WorkflowInfo, node: Workflow.WorkflowNode) {
  const startNode = workflow.nodes.find(item => item.wfComponent.name === 'Start')
  if (!startNode)
    throw new Error('Start node not found')

  const firstInput = startNode.inputConfig.user_inputs[0]
  node.nodeConfig = {
    default_target_node_uuid: '',
    cases: [
      {
        uuid: uuidv4().replace(/-/g, ''),
        operator: 'and',
        target_node_uuid: '',
        conditions:
          [
            { node_uuid: startNode.uuid, node_param_name: firstInput.name, operator: 'contains', value: '' },
          ],
      },
      {
        uuid: uuidv4().replace(/-/g, ''),
        operator: 'and',
        target_node_uuid: '',
        conditions:
          [
            { node_uuid: startNode.uuid, node_param_name: firstInput.name, operator: 'contains', value: '' },
          ],
      },
    ],
  }
}

function createClassifierNode(node: Workflow.WorkflowNode) {
  const appStore = useAppStore()
  node.nodeConfig = {
    model_name: appStore.getFirstLLM().modelName,
    categories: [
      {
        category_uuid: uuidv4().replace(/-/g, ''),
        category_name: '',
        target_node_uuid: '',
      },
      {
        category_uuid: uuidv4().replace(/-/g, ''),
        category_name: '',
        target_node_uuid: '',
      },
    ],
  }
}

function createAnswer(node: Workflow.WorkflowNode) {
  const appStore = useAppStore()
  node.nodeConfig = {
    prompt: '',
    model_name: appStore.getFirstLLM().modelName,
  }
}

function createKeywordExtractor(node: Workflow.WorkflowNode) {
  const appStore = useAppStore()
  node.nodeConfig = {
    top_n: 5,
    model_name: appStore.getFirstLLM().modelName,
  }
}

function createFaqExtractor(node: Workflow.WorkflowNode) {
  const appStore = useAppStore()
  node.nodeConfig = {
    top_n: 5,
    model_name: appStore.getFirstLLM().modelName,
  }
}

function createKnowledgeRetrieval(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    knowledge_base_uuid: '',
    knowledge_base_name: '',
    score: 0.6,
    top_n: 3,
    is_strict: true,
    default_response: '',
  }
}

function createOpenAiImage(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    prompt: '',
    size: '1024x1024',
    quality: 'standard',
  }
}

function createTongyiwanx(node: Workflow.WorkflowNode) {
  const appStore = useAppStore()
  const opts = appStore.imageModels.filter((item) => {
    if (item.modelName.includes('wanx-background'))
      return false

    return item.modelPlatform === 'dashscope'
  })
  node.nodeConfig = {
    model_name: opts.length > 0 ? opts[0].modelName : '',
    prompt: '',
    size: '1024*1024',
    seed: -1,
  }
}

function createGoogle(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    query: '',
    country: 'cn',
    language: 'zh-cn',
    top_n: 5,
  }
}

function createHumanFeedback(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    tip: '',
  }
}

function createMailSend(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    sender_type: 1,
    cc_mails: '',
    to_mails: '',
    subject: '',
    content: '',
    smtp: {
      host: '',
      port: 465,
    },
    sender: {
      name: '',
      mail: '',
      password: '',
    },
  }
}

function createHttpRequest(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    method: 'GET',
    url: '',
    content_type: 'text/plain', // text/plain, application/json, application/x-www-form-urlencoded, multipart/form-data
    headers: [{ name: 'Accept', value: '*/*' }, { name: 'Cache-Control', value: 'no-cache' }, { name: 'Connection', value: 'keep-alive' }],
    params: [],
    text_body: '',
    json_body: {},
    form_data_body: [],
    form_urlencoded_body: [],
    body: {},
    timeout: 10,
    retry_times: 0,
    clear_html: false,
  }
}

function createAgent(node: Workflow.WorkflowNode) {
  node.nodeConfig = {
    character_uuid: '',
    enable_rag: true,
    enable_mcp: true,
    enable_web_search: false,
  }
}

export function getInputLabelFromParamName(workflow: Workflow.WorkflowInfo, nodeUuid: string, nodeParamName: string) {
  const node = workflow.nodes.find(node => node.uuid === nodeUuid)
  if (!node)
    return ''

  if (node.wfComponent.name === 'Start') {
    const inputConfig = node.inputConfig
    const input = inputConfig.user_inputs.find(input => input.name === nodeParamName)
    if (input)
      return input.title
  } else {
    return node.title
  }
}

export function getNameByInputType(type: number) {
  switch (type) {
    case 1:
      return '文本'
    case 2:
      return '数字'
    case 3:
      return '下拉选项'
    case 4:
      return '文件列表'
    default:
      return 'Unknown'
  }
}

export function getIconByComponentName(name: string) {
  switch (name.toLowerCase()) {
    case 'answer':
      return 'carbon:question-answering'
    case 'classifier':
      return 'carbon:type-pattern'
    case 'knowledgeretrieval':
      return 'carbon:connect-target'
    case 'documentextractor':
      return 'carbon:ibm-knowledge-catalog-standard'
    case 'keywordextractor':
      return 'carbon:api-key'
    case 'faqextractor':
      return 'fluent-mdl2:book-answers'
    case 'switcher':
      return 'oui:logstash-if'
    case 'template':
      return 'carbon:prompt-template'
    case 'openaiimage':
      return 'solar:pallete-2-linear'
    case 'tongyiwanx':
      return 'solar:pallete-2-linear'
    case 'google':
      return 'ri:google-line'
    case 'humanfeedback':
      return 'covid:transmission-virus-human-transmit-2'
    case 'mailsend':
      return 'carbon:mail-all'
    case 'httprequest':
      return 'carbon:http'
    case 'agent':
      return 'carbon:bot'
    case 'end':
      return 'carbon:closed-caption'
    case 'start':
      return 'carbon:play-outline'
    default:
      return ''
  }
}

export function getIconClassByComponentName(name: string) {
  switch (name.toLowerCase()) {
    case 'answer':
      return 'text-green-800'
    case 'classifier':
      return 'text-violet-900'
    case 'knowledgeretrieval':
      return 'text-stone-900'
    case 'documentextractor':
      return 'text-rose-900'
    case 'keywordextractor':
      return 'text-cyan-900'
    case 'faqextractor':
      return 'text-teal-600'
    case 'switcher':
      return 'text-yellow-900'
    case 'template':
      return 'text-sky-800'
    case 'openaiimage':
      return 'text-fuchsia-700'
    case 'tongyiwanx':
      return 'text-fuchsia-700'
    case 'google':
      return 'text-emerald-900'
    case 'humanfeedback':
      return 'text-zinc-800'
    case 'mailsend':
      return 'text-amber-800'
    case 'httprequest':
      return 'text-slate-800'
    case 'agent':
      return 'text-indigo-800'
    case 'end':
      return 'text-orange-800'
    case 'start':
      return 'text-blue-900'
    default:
      return ''
  }
}
