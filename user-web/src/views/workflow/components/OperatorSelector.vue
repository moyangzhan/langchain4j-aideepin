<script setup lang="ts">
import { ref } from 'vue'
import { NSelect } from 'naive-ui'
import type { SelectGroupOption, SelectOption } from 'naive-ui'
import { useWfStore } from '@/store'

interface Props {
  selected: string
}
const props = withDefaults(defineProps<Props>(), {
  selected: () => '',
})

const emit = defineEmits<Emit>()
interface Emit {
  (e: 'operatorSelected', uuid: string): void
}
const wfStore = useWfStore()

const selected = ref<string>(props.selected || '')
const options: Array<SelectOption | SelectGroupOption> = []
for (let i = 0; i < wfStore.operators.length; i++) {
  const operator = wfStore.operators[i]
  options.push({
    label: operator.desc,
    value: operator.name,
  })
}

// if(options.length > 0) {
//   selected.value = options[0].key as string
// }

function handleSelect(value: string) {
  console.log('node selected', value)
  emit('operatorSelected', value)
}
</script>

<template>
  <NSelect
    v-model:value="selected" placement="top-start" trigger="click" :show-arrow="true" :options="options"
    :consistent-menu-width="false" @update:value="handleSelect"
  />
</template>
