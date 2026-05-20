declare namespace Workflow {
  import type { Node, Edge } from '@vue-flow/core'

  interface WorkflowComponent {
    id: string
    uuid: string
    name: string
    title: string
    remark: string
    displayOrder: number
    isEnable: boolean
  }

  interface WorkflowNode {
    id: string
    uuid: string
    workflowId: string
    workflowComponentId: string
    title: string
    remark: string
    inputConfig: { user_inputs: NodeIODefinition[], ref_inputs: NodeIORefDinition[] }
    nodeConfig: NodeConfig
    outputConfig: object
    positionX: number
    positionY: number

    workflowUuid: string
    wfComponent: WorkflowComponent
    sourceHandleIds: string[]
    targetHandleIds: string[]
  }

  interface WorkflowEdge {
    id: string
    uuid: string
    workflowId: string
    sourceNodeUuid: string
    sourceHandle: string
    targetNodeUuid: string

    workflowUuid: string
  }

  // 工作流信息
  interface WorkflowInfo {
    id: string
    uuid: string
    title: string
    remark: string
    isPublic: boolean
    nodes: WorkflowNode[]
    edges: WorkflowEdge[]
    userId: string
    userUuid: string
    userName: string
    createTime: string

    deleteNodes: string[]
    deleteEdges: string[]
  }

  interface WorkflowUpdateReq {
    uuid: string
    nodes: WorkflowNode[]
    edges: WorkflowEdge[]
    deleteNodes: string[]
    deleteEdges: string[]
  }

  // 工作流运行时信息
  interface WorkflowRuntime {
    id: string
    uuid: string
    workflowId: string
    input: any
    output: any
    status: number
    statusRemark: string
    createTime: string
    loading: boolean

    wfUuid: string
    nodes: WfRuntimeNode[]
  }

  interface WfRuntimeNode {
    id: string
    uuid: string
    workflowRuntimeId: string
    nodeId: string
    //json object
    input: any
    //json object
    output: any
    status: number
    statusRemark: string
    createTime: string

    wfComponent: WorkflowComponent
    wfRuntimeUuid: string
    nodeUuid: string
    nodeTitle: string
  }

  interface WorkflowState {
    showCreateOrEditView: boolean
    createOrEditWfUuid: string
    selectedType: string
    activeWorkflowInfo: WorkflowInfo
    wfUuidToUIWorkflow: Map<string, UIWorkflow>
    activeUuid: string
    wfComponents: WorkflowComponent[]
    myWorkflows: WorkflowInfo[]
    publicWorkflows: WorkflowInfo[]
    loadingMyWorkflows: boolean,
    loadingPublicWorkflows: boolean,
    wfUuidToWfRuntimeLoading: Map<string, boolean>
    wfUuidToWfRuntimes: Map<string, WorkflowRuntime[]>
    operators: Operator[]
    submitting: boolean
  }

  interface InfoListResp {
    total: number,
    records: WorkflowInfo[]
  }

  interface WfRuntimesResp {
    total: number,
    records: WorkflowRuntime[]
  }

  interface NodeIODefinition {
    uuid: string
    type: number
    name: string
    title: string
    required: boolean

    //type === files
    limit: number
    //type === options
    multiple: boolean
  }

  //引用类型的输入输出定义
  interface NodeIORefDinition {
    uuid: string
    name: string
    node_param_name: string
    node_uuid: string
  }

  interface UserInput {
    uuid: string
    name: string
    content: UserInputContent

    required: boolean
  }

  interface UserInputContent {
    type: number
    value: any
    title: string
  }

  interface NodeIOData {
    title: string
    type: number
    value: any
  }

  interface NodeConfig {
  }

  interface NodeConfigStart implements NodeConfig {
  }

  interface NodeConfigAnswer implements NodeConfig {
    model_platform: string
    model_name: string
    prompt: string
  }

  //Classifier node
  interface NodeConfigClassifier implements NodeConfig {
    model_platform: string
    model_name: string
    categories: NodeConfigClassifierCategory[]
  }

  interface NodeConfigClassifierCategory {
    target_node_uuid: string
    category_uuid: string
    category_name: string
  }

  //Switcher node
  interface NodeConfigSwitcher implements NodeConfig {
    default_target_node_uuid: string
    cases: NodeConfigSwitcherCase[]
  }

  interface NodeConfigTemplate implements NodeConfig {
    template: string
  }

  interface NodeConfigEnd implements NodeConfig {
    result: string
  }

  interface NodeConfigKnowledgeRetrieval implements NodeConfig {
    knowledge_base_uuid: string
    knowledge_base_name: string
    score: number
    top_n: number
    is_strict: boolean
    default_response: string
  }

  interface NodeConfigSwitcherCase {
    uuid: string
    operator: string
    target_node_uuid: string
    conditions: NOdeConfigCaseCondition[]
  }

  interface NodeConfigSwitcherCaseCondition {
    uuid: string
    node_uuid: string
    node_param_name: string
    operator: string
    value: string
  }

  interface NodeConfigKeywordExtractor implements NodeConfig {
    model_platform: string
    model_name: string
    top_n: number
  }

  interface NodeConfigFaqExtractor implements NodeConfig {
    model_platform: string
    model_name: string
    top_n: number
  }

  interface NodeConfigGoogleSearch implements NodeConfig {
    query: string
    country: string
    language: string
    top_n: number
  }

  interface NodeConfigOpenAiImage implements NodeConfig {
    prompt: string
    size: string
    quality: string
  }

  interface NodeConfigTongyiwanx implements NodeConfig {
    model_name: string
    prompt: string
    size: string
    seed: number
  }

  interface NodeConfigHumanFeedback implements NodeConfig {
    tip: string
  }

  interface NodeConfigMailSend implements NodeConfig {
    sender_type: number
    cc_mails: string
    to_mails: string
    subject: string
    content: string
    smtp: {
      host: string
      port: number
    }
    sender: {
      name: string
      mail: string
      password: string
    }
  }

  interface NodeConfigHttpRequestParam {
    name: string
    value: string
  }

  interface NodeConfigHttpRequest implements NodeConfig {
    method: string
    url: string
    headers: NodeConfigHttpRequestParam[]
    params: NodeConfigHttpRequestParam[]
    text_body: string
    json_body: object
    form_data_body: NodeConfigHttpRequestParam[]
    form_urlencoded_body: NodeConfigHttpRequestParam[]
    content_type: string
    timeout: number
    retry_times: number
    clear_html: boolean
  }

  interface NodeIOData {
    name: string
    value: any
  }

  interface Operator {
    name: string
    desc: string
  }

  interface UIWorkflow {
    nodes: Node[]
    edges: Edge[]
  }

  interface InputLabel {
    label: string
    value: string
  }
}