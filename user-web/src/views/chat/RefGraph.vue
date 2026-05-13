<script setup lang='ts'>
import { nextTick, onMounted, onUpdated, ref } from 'vue'
import { NButton, NDivider, NFlex } from 'naive-ui'
import cytoscape from 'cytoscape'
import { useChatStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
interface Props {
  msgUuid: string
}
const props = withDefaults(defineProps<Props>(), {
  msgUuid: '',
})
const chatStore = useChatStore()
const loading = ref<boolean>(false)
const isEmpty = ref<boolean>(false)
const selectedVertex = ref<KnowledgeBase.KbVertex | null>()
const selectedEdge = ref<KnowledgeBase.KbEdge | null>()
const graphRef = ref<KnowledgeBase.QaRecordGraphRef | null>({ edges: [], vertices: [] })
let cy: any = null

function getAndRenderGraph() {
  graphRef.value = chatStore.getGraphRef(props.msgUuid)
  if (!graphRef.value || (graphRef.value.vertices.length === 0 && graphRef.value.edges.length === 0))
    loadGraph()
  else
    parseAndRender(graphRef.value)
}

function parseAndRender(graphRef: KnowledgeBase.QaRecordGraphRef) {
  cy.$('node').remove()
  cy.$('edge').remove()
  const nodes = graphRef.vertices.map((item) => {
    return { group: 'nodes', data: { id: `${item.id}`, name: item.name, description: item.description } }
  })
  const edges = graphRef.edges.map((item) => {
    return { group: 'edges', data: { id: `${item.id}`, label: `${item.label}`, source: `${item.startId}`, target: `${item.endId}`, description: item.description } }
  })
  renderGraph(nodes, edges)
}

function renderGraph(nodes: any, edges: any) {
  if (nodes.length > 0) {
    cy.add(nodes)
    cy.nodes().on('click', (e: any) => {
      const clickedNode = e.target
      selectedVertex.value = clickedNode.data()
      selectedEdge.value = null
    })
  }
  if (edges.length > 0) {
    cy.add(edges)
    cy.edges().on('click', (e: any) => {
      const clickedNode = e.target
      selectedVertex.value = null
      selectedEdge.value = clickedNode.data()
    })
  }
  nextTick(() => {
    cy.resize()
    relayout()
  })
}

async function loadGraph() {
  const curQaRecordUuid = props.msgUuid
  if (chatStore.isLoadingGraphRef(curQaRecordUuid))
    return

  chatStore.setLoadingGraphRef(curQaRecordUuid, true)
  try {
    const resp = await api.messageGraphRef<KnowledgeBase.KbItemGraphResp>(curQaRecordUuid)
    if (resp.data)
      chatStore.setKnowledgeGraphRef(curQaRecordUuid, { ...resp.data })
  } finally {
    chatStore.setLoadingGraphRef(curQaRecordUuid, false)

    // 加载结束后判断是否还停留在加载时的页面，是的话则渲染图形
    if (curQaRecordUuid === props.msgUuid) {
      const loadedRef = chatStore.getGraphRef(curQaRecordUuid)
      if (loadedRef)
        parseAndRender(loadedRef)
    }

    loading.value = chatStore.isLoadingGraphRef(props.msgUuid)
  }
}

function initCy() {
  console.log('ref graph initCy')
  cy = cytoscape({
    container: document.getElementById('refGraphCy'),
    elements: [],
    style: [
      {
        selector: 'node',
        style: {
          content: 'data(name)',
          width: 30,
          height: 30,
        },
      },
    ],
  })
}

function relayout() {
  const layout = cy.layout({
    name: 'cose',
  })
  layout.run()
  isEmpty.value = cy.elements().length === 0
}

onUpdated(() => {
  console.log('RefGraph onUpdated')
  nextTick(() => {
    selectedVertex.value = null
    selectedEdge.value = null
    getAndRenderGraph()
  })
})

onMounted(() => {
  console.log('RefGraph onMounted')
  nextTick(() => {
    initCy()
    getAndRenderGraph()
  })
})
</script>

<template>
  <NFlex>
    <div id="refGraphCy" style="width:80%; height: 400px;" class="border border-gray-300" />
    <div class="w-1/6 h-[400px] overflow-y-auto">
      <NButton v-show="!isEmpty" size="small" :loading="loading" type="info" ghost @click="relayout">
        {{ t('chat.relayout') }}
      </NButton>
      <NButton v-show="isEmpty" size="small" type="warning" ghost>
        {{ t('common.noData') }}
      </NButton>
      <NFlex v-if="selectedVertex" vertical>
        <NDivider title-placement="left">
          {{ t('chat.entity') }}
        </NDivider>
        <div>{{ selectedVertex.id }}</div>
        <NDivider title-placement="left">
          {{ t('common.name') }}
        </NDivider>
        <div>{{ selectedVertex.name }}</div>
        <NDivider title-placement="left">
          {{ t('common.description') }}
        </NDivider>
        <div>{{ selectedVertex.description }}</div>
      </NFlex>
      <NFlex v-if="selectedEdge" vertical>
        <NDivider title-placement="left">
          {{ t('chat.relation') }}
        </NDivider>
        <div>{{ selectedEdge.id }}</div>
        <NDivider title-placement="left">
          {{ t('common.description') }}
        </NDivider>
        <div>{{ selectedEdge.description }}</div>
      </NFlex>
    </div>
  </NFlex>
</template>
