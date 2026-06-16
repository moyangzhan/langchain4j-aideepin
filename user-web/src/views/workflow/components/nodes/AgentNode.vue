<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { NodeProps } from '@vue-flow/core'
import CommonNodeHeader from '../CommonNodeHeader.vue'
import api from '@/api'

// Module-scoped cache shared by every AgentNode card on the canvas — fetched once
// per session and reused. Only stores the (uuid → title) mapping the card needs;
// the full character object is loaded on demand by the property panel.
const characterTitles = ref<Map<string, string> | null>(null)
let inflight: Promise<void> | null = null

async function ensureCharacters() {
  if (characterTitles.value || inflight)
    return inflight ?? undefined
  inflight = (async () => {
    try {
      const { data } = await api.fetchCharacters<{ uuid: string, title: string }[]>()
      const map = new Map<string, string>()
      if (Array.isArray(data))
        data.forEach(c => map.set(c.uuid, c.title))
      characterTitles.value = map
    } catch (e) {
      console.error('AgentNode: failed to load character titles', e)
      characterTitles.value = new Map() // mark as resolved-empty so we don't retry on every node
    } finally {
      inflight = null
    }
  })()
  return inflight
}

const props = defineProps<NodeProps>()
const characterUuid = computed(() => (props.data.nodeConfig as Workflow.NodeConfigAgent).character_uuid || '')
const characterTitle = computed(() => {
  const uuid = characterUuid.value
  if (!uuid)
    return ''
  return characterTitles.value?.get(uuid) || uuid.slice(0, 8) // short fallback while loading
})

onMounted(ensureCharacters)
</script>

<template>
  <div class="flex flex-col w-full">
    <Handle type="target" :position="Position.Left" />
    <Handle type="source" :position="Position.Right" />
    <CommonNodeHeader :wf-node="data" />
    <div class="flex-1 flex-col">
      <div class="content_line flex items-center">
        <span class="iconify mx-1.5" data-icon="carbon:bot" />
        <span class="truncate" :title="characterTitle">{{ characterTitle }}</span>
      </div>
    </div>
  </div>
</template>
