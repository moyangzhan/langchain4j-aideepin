<script setup lang='ts'>
import { nextTick, onMounted, onUpdated, ref } from 'vue'
import { NButton, NDivider, NFlex } from 'naive-ui'
import cytoscape from 'cytoscape'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  kbItemUuid: string
}
const props = withDefaults(defineProps<Props>(), {
  kbItemUuid: '',
})
const limit = 100
const loading = ref<boolean>(false)
const vertexCount = ref<number>(0)
const selectedVertex = ref<KnowledgeBase.KbVertex | null>()
const selectedEdge = ref<KnowledgeBase.KbEdge | null>()
const isEmpty = ref<boolean>(false)
let cy: any = null

async function loadGraph(maxVertexId: number, maxEdgeId: number) {
  if (loading.value)
    return

  if (!props.kbItemUuid) {
    console.log('loadGraph kbItemUuid is empty')
    return
  }

  loading.value = true
  try {
    cy.$('node').remove()
    cy.$('edge').remove()
    const resp = await api.knowledgeBaseGraph<KnowledgeBase.KbItemGraphResp>(props.kbItemUuid, maxVertexId, maxEdgeId, limit)
    vertexCount.value = resp.data.vertices.length
    const nodes = resp.data.vertices.map((item) => {
      return { group: 'nodes', data: item }
    })
    const edges = resp.data.edges.map((item) => {
      return { group: 'edges', data: { source: `${item.startId}`, target: `${item.endId}`, ...item } }
    })
    renderGraph(nodes, edges)
  } finally {
    loading.value = false
  }
}

onUpdated(() => {
  console.log('ItemGraph onUpdated')
  nextTick(() => {
    loadGraph(Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER)
  })
})

onMounted(() => {
  console.log('ItemGraph onMounted')
  nextTick(() => {
    initCy()
    loadGraph(Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER)
  })
})

function initCy() {
  cy = cytoscape({
    container: document.getElementById('itemGraphCy'),
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

function renderGraph(nodes: any, edges: any) {
  console.log('renderGraph')
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
  relayout()
  isEmpty.value = cy.elements().length === 0
}

function relayout() {
  const options = {
    name: 'cose',
    // Called on `layoutready`
    ready() { },
    // Called on `layoutstop`
    stop() { },
    // Whether to animate while running the layout
    // true : Animate continuously as the layout is running
    // false : Just show the end result
    // 'end' : Animate with the end result, from the initial positions to the end positions
    animate: true,
    // Easing of the animation for animate:'end'
    animationEasing: undefined,
    // The duration of the animation for animate:'end'
    animationDuration: undefined,
    animateFilter(node: any, i: any) { return true },
    // The layout animates only after this many milliseconds for animate:true
    // (prevents flashing on fast runs)
    animationThreshold: 250,
    refresh: 20,
    // Whether to fit the network view after when done
    fit: true,
    padding: 30,
    boundingBox: undefined,
    nodeDimensionsIncludeLabels: false,
    randomize: false,
    componentSpacing: 40,
    nodeRepulsion(node: any) { return 2048 },
    nodeOverlap: 4,
    idealEdgeLength(edge: any) { return 32 },
    edgeElasticity(edge: any) { return 32 },
    nestingFactor: 1.2,
    gravity: 1,
    numIter: 1000,
    initialTemp: 1000,
    coolingFactor: 0.99,
    minTemp: 1.0,
  }
  const layout = cy.layout(options)
  layout.run()
}
</script>

<template>
  <NFlex>
    <div id="itemGraphCy" style="width:80%; height: 400px;" class="border border-gray-300" />
    <div class="w-1/6 h-[400px] overflow-y-auto">
      <NButton v-show="!isEmpty" size="small" :loading="loading" type="info" ghost @click="relayout">
        {{ t('workflow.relayout') }}
      </NButton>
      <NButton v-show="isEmpty" size="small" type="warning" ghost>
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
