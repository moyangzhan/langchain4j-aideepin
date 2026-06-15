import { format } from 'date-fns'
import type { Edge, Node } from '@vue-flow/core'
import { CHAT_MESSAGE_CONTENT_TYPE } from '@/utils/constant'

export function getCurrentDate() {
  const date = new Date()
  const day = date.getDate()
  const month = date.getMonth() + 1
  const year = date.getFullYear()
  return `${year}-${month}-${day}`
}

export function knowledgeBaseEmptyInfo() {
  return {
    id: '0',
    uuid: '',
    title: '',
    remark: '',
    isPublic: false,
    isStrict: true,
    starCount: 0,
    ownerUuid: '',
    ownerName: '',
    itemCount: 0,
    embeddingCount: 0,
    ingestMaxOverlap: 0,
    ingestSplitStrategy: 'recursive',
    ingestMaxSegmentSize: 1000,
    ingestCustomSeparator: '',
    ingestModelId: '0',
    ingestTokenEstimator: '',
    retrieveMaxResults: 0,
    retrieveMinScore: 0,
    queryLlmTemperature: 0,
    querySystemMessage: '',

    ingestModelName: '',
  }
}

export function knowledgeBaseEmptyItem() {
  return {
    id: '0',
    uuid: '',
    kbId: '0',
    kbUuid: '',
    title: '',
    brief: '',
    remark: '',
    embeddingStatus: 'NONE',
    graphicalStatus: 'NONE',
    embeddingStatusChangeTime: '',
    graphicalStatusChangeTime: '',
    sourceFileName: '',
    sourceFileUrl: '',
    sourceFileUuid: '',
  }
}

export function knowledgeBaseEmptyRecord() {
  return {
    id: '0',
    uuid: '',
    kbId: '0',
    kbUuid: '',
    question: '',
    answer: '',
    createTime: format(new Date(), 'yyyy-MM-dd HH:mm:ss'),
    aiModelPlatform: '',
    loading: false,
    error: false,
  }
}

export function emptyAiModel() {
  return {
    // from api
    modelId: '',
    modelName: '',
    modelTitle: '',
    modelPlatform: '',
    enable: false,
    isFree: false,
    type: 'text',
    inputTypes: 'text',
    isReasoner: false,
    isThinkingClosable: false,
    isSupportWebSearch: false,
    properties: {} as object,

    // for NSelector
    value: '',
    key: '',
    label: '',
    disabled: false,
  }
}

export function emptyDraw() {
  return {
    id: 0,
    uuid: '',
    prompt: '',
    createTime: format(new Date(), 'yyyy-MM-dd HH:mm:ss'),
    interactingMethod: 1,
    processStatus: 1,
    processStatusRemark: '',
    originalImageUuid: '',
    originalImageUrl: '',
    maskImageUuid: '',
    maskImageUrl: '',
    imageUuids: [],
    imageUrls: [],
    isPublic: false,
    isStar: false,
    aiModelName: '',
    aiModelPlatform: '',
    starCount: 0,
    userUuid: '',
    userName: '',
    dynamicParams: {} as any,
    duration: 0,
  }
}

export function emptyWorkflowInfo() {
  return {
    id: '',
    uuid: '',
    title: '',
    remark: '',
    createTime: format(new Date(), 'yyyy-MM-dd HH:mm:ss'),
    input: {} as any,
    output: {} as any,
    isPublic: false,
    userId: '',
    userUuid: '',
    userName: '',
    nodes: [] as Workflow.WorkflowNode[],
    edges: [] as Workflow.WorkflowEdge[],
    deleteNodes: [],
    deleteEdges: [],
  }
}

export function emptyUiWorkflow() {
  return {
    nodes: [] as Node[],
    edges: [] as Edge[],
  }
}

export function emptyWorkflowNode() {
  return {
    id: '',
    uuid: '',
    workflowId: '',
    workflowComponentId: '',
    title: '',
    remark: '',
    inputConfig: { user_inputs: [], ref_inputs: [] } as any,
    nodeConfig: {} as any,
    outputConfig: {} as any,
    positionX: 0,
    positionY: 0,

    workflowUuid: '',
    wfComponent: {} as any,
    sourceHandleIds: [],
    targetHandleIds: [],
  }
}

export function emptyWfRuntime() {
  return {
    id: '',
    uuid: '',
    workflowId: '',
    createTime: format(new Date(), 'yyyy-MM-dd HH:mm:ss'),
    status: 1,
    statusRemark: '',
    input: {} as any,
    output: {} as any,
    nodes: [] as Workflow.WfRuntimeNode[],

    workflowUuid: '',
    error: false,
    loading: false,
  }
}

export function emptyWfNodeRuntime() {
  return {
    id: '',
    uuid: '',
    workflowRuntimeId: '',
    input: {},
    output: {},
    status: 1,
    statusRemark: '',
    createTime: '',

    wfRuntimeUuid: '',
    error: false,
  }
}

export function emptyCharacter(): Chat.Character {
  return {
    uuid: '',
    title: '',
    remark: '',
    aiSystemMessage: '',
    understandContextEnable: false,
    loadedAll: false,
    loadedFirstPageMsg: false,
    minMsgUuid: '',
    mcpIds: [],
    kbIds: [], // 关联的知识库ID
    characterKnowledgeList: [], // 关联的知识库
    answerContentType: CHAT_MESSAGE_CONTENT_TYPE.auto, // 1: auto, 2: text, 3: audio
    isAutoplayAnswer: false, // 聊天时音频类型的响应内容是否自动播放
    isEnableThinking: false, // 是否启用思考过程
    isEnableWebSearch: false, // 是否启用网络搜索
    audioConfig: {
      voice: {
        param_name: '',
        model: '',
        platform: '',
      },
    }, // 语音配置
  }
}

export function emptyChatMessage(): Chat.ChatMessage {
  return {
    uuid: '',
    createTime: '',
    thinkingContent: '',
    remark: '',
    audioUuid: '',
    audioUrl: '',
    audioDuration: 0, // in seconds
    messageRole: 0,
    children: [{ // for reply
      uuid: '',
      createTime: '',
      thinkingContent: '',
      remark: '',
      audioUuid: '',
      audioUrl: '',
      audioDuration: 0,
      messageRole: 0,
      children: [],
      aiModelPlatform: '',
      attachmentUrls: [],
      isRefEmbedding: false,
      isRefGraph: false,
      isRefMemoryEmbedding: false,
      audioPlayState: emptyAudioPlayState(),
      contentType: CHAT_MESSAGE_CONTENT_TYPE.text, // 2: text, 3: audio
    }], // for reply
    aiModelPlatform: '',
    attachmentUrls: [],
    isRefEmbedding: false,
    isRefGraph: false,
    isRefMemoryEmbedding: false,
    audioPlayState: emptyAudioPlayState(),
    contentType: CHAT_MESSAGE_CONTENT_TYPE.text, // 2: text, 3: audio
  }
}

export function calcImageUrls(draw: Chat.Draw) {
  // draw.imageUrls = draw.imageUuids.map((item) => {
  //   if (draw.isPublic)
  //     return `/draw/public/image/${draw.uuid}/${item}`
  //   else
  //     return `/image/${item}`
  // })
  for (let i = 0; i < draw.imageUrls.length; i++) {
    const url = draw.imageUrls[i]
    if (!url.includes('http')) {
      if (draw.isPublic)
        draw.imageUrls[i] = `/api/draw/public/image/${draw.uuid}/${changeFileUrlToUuid(url)}`
      else
        draw.imageUrls[i] = `/api${url}`
    }
  }
}

export function getRealFileUrl(fileUrl: string) {
  if (!fileUrl.includes('http') && !fileUrl.includes('/api'))
    return `/api${fileUrl}`
  else
    return fileUrl
}

export function emptyQuota(): User.Config {
  return {
    userQuota: {
      requestTimesByDay: 0,
      requestTimesByMonth: 0,
      tokenByDay: 0,
      tokenByMonth: 0,
      drawByDay: 0,
      drawByMonth: 0,
    },
    quotaCost: {
      paidRequestTimes: {
        todayRequestTimes: 0,
        monthRequestTimes: 0,
      },
      paidTokenCost: {
        todayTokenCost: 0,
        monthTokenCost: 0,
      },
      paidDrawTimes: {
        todayDrawTimes: 0,
        monthDrawTimes: 0,
      },
      freeTokenCost: {
        todayTokenCost: 0,
        monthTokenCost: 0,
      },
      freeRequestTimes: {
        todayRequestTimes: 0,
        monthRequestTimes: 0,
      },
      freeDrawTimes: {
        todayDrawTimes: 0,
        monthDrawTimes: 0,
      },
    },
  }
}

export function emptyMcp(): Mcp.McpInfo {
  return {
    id: '',
    uuid: '',
    title: '',
    transportType: '',
    sseUrl: '',
    sseTimeout: 0,
    stdioCommand: '',
    stdioArg: '',
    presetParams: [],
    customizedParamDefinitions: [],
    installType: 'git',
    website: '',
    remark: '',
    isEnable: false,
    configured: false, // 是否已配置
  }
}

export function emptyUserMcp(): Mcp.UserMcp {
  return {
    id: '',
    uuid: '',
    userId: '',
    mcpId: '',
    mcpCustomizedParams: [],
    isEnable: false,
    mcpInfo: emptyMcp(), // mcp信息
  }
}

export function changeFileUrlToUuid(fileUrl: string) {
  if (!fileUrl.includes('/'))
    return fileUrl
  return fileUrl.substring(fileUrl.lastIndexOf('/') + 1)
}

export function fillCompleteUrl(fileUrl: string) {
  if (!fileUrl.includes('/') || fileUrl.indexOf('/api') === 0)
    return fileUrl
  return `/api${fileUrl}`
}

export function emptyAudioPlayState(): AudioPlayState {
  return {
    audioUrl: '',
    playDuration: 0,
    playing: false,
    audio: null, // Audio object
    text: '', // Text to be read
    showText: false, // Whether to show the text
    audioUuid: '', // UUID of the audio file
    msgPart: '', // 聊天时不断接收到的消息片段，即时播放时使用
    audioFrame: '', // audio frame
  }
}

export function emptyAsrSetting(): AsrSetting {
  return {
    model_name: '', // ASR model name
    platform: '', // ASR platform, eg: local or remote
    max_record_duration: 60, // Maximum recording duration in seconds
    max_file_size: 10 * 1024 * 1024, // Maximum file
  }
}

export function emptyTtsSetting(): TtsSetting {
  return {
    synthesizer_side: 'client', // client | server
    model_name: '', // TTS model name
    platform: '', // TTS platform, eg: local or remote
  }
}

export function emptySysConfigInfo(): SysConfigInfo {
  return {
    asrSetting: emptyAsrSetting(),
    ttsSetting: emptyTtsSetting(),
    availableVoices: [],
    defaultLocale: 'zh-CN',
  }
}
