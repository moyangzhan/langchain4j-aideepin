<script setup lang='ts'>
import { computed, nextTick, onActivated, onDeactivated, onMounted, onUnmounted, ref, watch } from 'vue'
import { NButton, NDivider, NFlex } from 'naive-ui'
import { useMessage } from 'naive-ui'
import cytoscape from 'cytoscape'
import api from '@/api'
import { t } from '@/locales'

const ms = useMessage()

interface Props {
  docUuid: string
  // Canvas and sidebar height: the standalone page passes a viewport-derived value
  height?: number
}
const props = withDefaults(defineProps<Props>(), {
  docUuid: '',
  height: 400,
})
const emit = defineEmits<Emit>()
interface Emit {
  (e: 'loaded', isEmpty: boolean): void
}
const limit = 100
const loading = ref<boolean>(false)
const selectedVertex = ref<KnowledgeBase.KbVertex | null>()
const selectedEdge = ref<KnowledgeBase.KbEdge | null>()
const isEmpty = ref<boolean>(false)
let cy: any = null

// Name-ordered cursor pagination: the cursor is the previous batch's last element (vertex
// name / edge endpoint pair); nodes and edges use the entity name as id (edges: source>target),
// "load more" appends incrementally with id-based dedup
const vertexCursor = ref<string>('')
const edgeCursor = ref<{ source: string; target: string } | null>(null)
const totalVertices = ref<number>(0)
const totalEdges = ref<number>(0)
const loadedVertices = ref<number>(0)
const loadedEdges = ref<number>(0)
const hasMore = computed(() => loadedVertices.value < totalVertices.value || loadedEdges.value < totalEdges.value)

// Request sequence + doc snapshot: a response only applies if it is still the latest request
// for the same document — a doc switch mid-flight must neither render the old doc's graph into
// the reset canvas nor silently drop the new doc's load. Concurrent duplicate loads (load-more
// double click) are harmless: they fetch the same cursor page and appendBatch dedupes by id.
let requestSeq = 0
async function loadGraph() {
  if (!props.docUuid)
    return
  const seq = ++requestSeq
  const docUuidAtRequest = props.docUuid

  loading.value = true
  try {
    const edgeCursorVal = edgeCursor.value
    const resp = await api.knowledgeBaseGraph<KnowledgeBase.KbItemGraphResp>(
      props.docUuid,
      limit,
      vertexCursor.value || undefined,
      edgeCursorVal?.source,
      edgeCursorVal?.target,
    )
    if (seq !== requestSeq || docUuidAtRequest !== props.docUuid)
      return
    totalVertices.value = resp.data.totalVertices ?? resp.data.vertices.length
    totalEdges.value = resp.data.totalEdges ?? resp.data.edges.length
    if (resp.data.vertices.length > 0)
      vertexCursor.value = resp.data.vertices[resp.data.vertices.length - 1].name
    if (resp.data.edges.length > 0)
      edgeCursor.value = { source: resp.data.edges[resp.data.edges.length - 1].sourceName, target: resp.data.edges[resp.data.edges.length - 1].targetName }
    appendBatch(resp.data.vertices, resp.data.edges)
  } catch (error: any) {
    if (seq === requestSeq)
      ms.error(error.message ?? 'error')
  } finally {
    loading.value = false
  }
}

function resetForDoc() {
  vertexCursor.value = ''
  edgeCursor.value = null
  totalVertices.value = 0
  totalEdges.value = 0
  loadedVertices.value = 0
  loadedEdges.value = 0
  selectedVertex.value = null
  selectedEdge.value = null
  cy?.$('node').remove()
  cy?.$('edge').remove()
}

// Reload only on document switch; onUpdated would refetch the whole graph whenever selecting
// a node re-renders the component
watch(() => props.docUuid, (val) => {
  if (val) {
    resetForDoc()
    nextTick(() => {
      loadGraph()
    })
  }
})

onMounted(() => {
  nextTick(() => {
    initCy()
    loadGraph()
  })
})

// KeepAlive caches one page per doc route: destroy the cytoscape instance on deactivation or
// every visited doc-graph page leaks a live one; re-activation rebuilds the canvas and reloads
// (skipped on the first activation right after mount, where onMounted already loaded)
onDeactivated(destroyCy)
onActivated(() => {
  nextTick(() => {
    if (cy)
      return
    initCy()
    resetForDoc()
    loadGraph()
  })
})
onUnmounted(destroyCy)

function destroyCy() {
  // also invalidate any in-flight load: its response would otherwise apply to a null canvas
  requestSeq++
  cy?.destroy()
  cy = null
}

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

function appendBatch(vertices: KnowledgeBase.KbVertex[], edges: KnowledgeBase.KbEdge[]) {
  addNodes(vertices.map(v => ({ id: v.name, name: v.name, description: v.description })))
  const edgeData = edges.map(e => ({
    id: `${e.sourceName}>${e.targetName}`,
    source: e.sourceName,
    target: e.targetName,
    sourceName: e.sourceName,
    targetName: e.targetName,
    description: e.description,
    weight: e.weight,
  }))
  // Vertices and edges page independently: an edge endpoint may not be loaded yet, so create
  // placeholder nodes (the ledger invariant guarantees endpoints are vertices; later vertex
  // batches dedupe by id)
  const endpointNames = [...new Set(edgeData.flatMap(e => [e.source, e.target]))]
  addNodes(endpointNames.map(name => ({ id: name, name })))
  if (edgeData.length > 0) {
    cy.add(edgeData.map(data => ({ group: 'edges', data }))).on('click', (e: any) => {
      selectedVertex.value = null
      selectedEdge.value = e.target.data()
    })
  }
  relayout()
  loadedVertices.value = cy.nodes().length
  loadedEdges.value = cy.edges().length
  isEmpty.value = cy.elements().length === 0
  emit('loaded', isEmpty.value)
}

function addNodes(list: { id: string; name: string; description?: string }[]) {
  const seen = new Set<string>()
  const fresh = list.filter((n) => {
    if (seen.has(n.id) || !cy.getElementById(n.id).empty())
      return false
    seen.add(n.id)
    return true
  })
  if (fresh.length === 0)
    return
  cy.add(fresh.map(data => ({ group: 'nodes', data }))).on('click', (e: any) => {
    selectedVertex.value = e.target.data()
    selectedEdge.value = null
  })
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
    // 'end' : Animate with the end result, from the initial positions to the final result
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
    <div class="flex flex-col" style="width:80%;">
      <div id="itemGraphCy" class="border border-gray-300" :style="{ height: `${height}px` }" />
      <!-- Pagination progress with the load-more entry under the canvas -->
      <div v-if="totalVertices > 0" class="flex items-center gap-3 mt-2">
        <span style="font-size: 12px; opacity: 0.7;">
          {{ t('knowledgeBase.graphPaginationInfo', { loadedVertices, totalVertices, loadedEdges, totalEdges }) }}
        </span>
        <NButton v-if="hasMore" size="tiny" type="info" ghost :loading="loading" @click="loadGraph">
          {{ t('knowledgeBase.loadMore') }}
        </NButton>
      </div>
    </div>
    <div class="w-1/6 overflow-y-auto" :style="{ height: `${height}px` }">
      <NButton size="small" :loading="loading" type="info" ghost @click="relayout">
        {{ t('workflow.relayout') }}
      </NButton>
      <NFlex v-if="selectedVertex" vertical>
        <NDivider title-placement="left">
          {{ t('workflow.entity') }}
        </NDivider>
        <div>{{ selectedVertex.name }}</div>
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
        <div>{{ selectedEdge.sourceName }} → {{ selectedEdge.targetName }}</div>
        <NDivider title-placement="left">
          {{ t('workflow.descriptionLabel') }}
        </NDivider>
        <div>{{ selectedEdge.description }}</div>
      </NFlex>
    </div>
  </NFlex>
</template>
