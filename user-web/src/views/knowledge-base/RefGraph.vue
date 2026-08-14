<script setup lang='ts'>
import { nextTick, onMounted, onUpdated, ref } from 'vue'
import { NButton, NDivider, NFlex, NSpin } from 'naive-ui'
import cytoscape from 'cytoscape'
import { useKbStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  qaRecordUuid: string
}
const props = withDefaults(defineProps<Props>(), {
  qaRecordUuid: '',
})
const kbStore = useKbStore()
const loading = ref<boolean>(false)
const isEmpty = ref<boolean>(false)
const selectedVertex = ref<KnowledgeBase.KbVertex | null>()
const selectedEdge = ref<KnowledgeBase.KbEdge | null>()
const graphRef = ref<KnowledgeBase.QaRecordGraphRef | null>({ edges: [], vertices: [] })
let cy: any = null

function getAndRenderGraph() {
  graphRef.value = kbStore.getGraphRef(props.qaRecordUuid)
  if (!graphRef.value)
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
  const curQaRecordUuid = props.qaRecordUuid
  if (kbStore.isLoadingGraphRef(curQaRecordUuid))
    return

  kbStore.setLoadingGraphRef(curQaRecordUuid, true)
  // 进入加载态：置位 loading 并清掉上一记录残留的"无数据"，避免加载期间显示空态
  loading.value = true
  isEmpty.value = false
  try {
    const resp = await api.knowledgeBaseGraphRef<KnowledgeBase.KbItemGraphResp>(curQaRecordUuid)
    if (resp.data)
      kbStore.setQaRecordGraphRef(curQaRecordUuid, { ...resp.data })
  } finally {
    kbStore.setLoadingGraphRef(curQaRecordUuid, false)
    loading.value = false

    // 加载结束后判断是否还停留在加载时的页面，是的话则渲染图形
    if (curQaRecordUuid === props.qaRecordUuid) {
      const loadedRef = kbStore.getGraphRef(curQaRecordUuid)
      if (loadedRef)
        parseAndRender(loadedRef)
      else
        isEmpty.value = true
    }
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
    <div class="relative border border-gray-300" style="width:80%; height: 400px;">
      <div id="refGraphCy" class="w-full h-full" />
      <div v-if="loading" class="absolute inset-0 flex items-start justify-center pt-4">
        <NSpin size="small" />
      </div>
    </div>
    <div class="w-1/6 h-[400px] overflow-y-auto">
      <NButton v-show="!loading && !isEmpty" size="small" :loading="loading" type="info" ghost @click="relayout">
        {{ t('workflow.relayout') }}
      </NButton>
      <NButton v-show="!loading && isEmpty" size="small" type="warning" ghost>
        {{ t('workflow.noData') }}
      </NButton>
      <NFlex v-if="selectedVertex" vertical>
        <NDivider title-placement="left">
          {{ t('workflow.entity') }}
        </NDivider>
        <div>{{ selectedVertex.id }}</div>
        <NDivider title-placement="left">
          {{ t('workflow.nameLabel') }}
        </NDivider>
        <div>{{ selectedVertex.name }}</div>
        <NDivider title-placement="left">
          {{ t('workflow.descriptionLabel') }}
        </NDivider>
        <div>{{ selectedVertex.description }}</div>
      </NFlex>
      <NFlex v-if="selectedEdge" vertical>
        <NDivider title-placement="left">
          {{ t('workflow.relation') }}
        </NDivider>
        <div>{{ selectedEdge.id }}</div>
        <NDivider title-placement="left">
          {{ t('workflow.descriptionLabel') }}
        </NDivider>
        <div>{{ selectedEdge.description }}</div>
      </NFlex>
    </div>
  </NFlex>
</template>
