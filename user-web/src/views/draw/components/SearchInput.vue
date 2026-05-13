<script setup lang='ts'>
import { computed, ref, watch } from 'vue'
import { storeToRefs } from 'pinia'
import { NAutoComplete, NButton, NCol, NInput, NRow } from 'naive-ui'
import { useAuthStore, useDrawStore, usePromptStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { debounce } from '@/utils/functions/debounce'
import { t } from '@/locales'
import api from '@/api'

interface Emit {
  (e: 'submit', prompt: string): void
}
const emit = defineEmits<Emit>()
const promptStore = usePromptStore()
const drawStore = useDrawStore()
const authStore = useAuthStore()
const { promptList: promptTemplate } = storeToRefs<any>(promptStore)
const prompt = ref<string>('')
const { isMobile } = useBasicLayout()

function getShow() {
  if (prompt.value.indexOf('/') === 0)
    return true

  return false
}

// autocomplete组件选中填充到输入框中的是label，只能通过这个方法把value的值填到输入框中
function handleSelect(val: any) {
  setTimeout(() => {
    prompt.value = val
  }, 0)
}

// inteacting method: Generate image
function handleEnter(event: KeyboardEvent) {
  if (!isMobile.value) {
    if (event.key === 'Enter' && !event.shiftKey) {
      event.preventDefault()
      handleSubmit()
    }
  } else {
    if (event.key === 'Enter' && event.ctrlKey) {
      event.preventDefault()
      handleSubmit()
    }
  }
}

function handleSubmit() {
  if (!authStore.token) {
    authStore.setLoginView(true)
    return
  }
  drawStore.setLoading(true)
  setTimeout(() => {
    drawStore.setLoading(false)
  }, 10000)
  emit('submit', prompt.value)
  prompt.value = ''
}

const handleSearchRemote = debounce(searchRemote, 300)
async function searchRemote() {
  const resp = await api.promptAutocomplete<Chat.Prompt[]>(prompt.value.replace('/', ''))
  promptTemplate.value = resp.data.map((item) => {
    return {
      label: item.act,
      value: item.prompt,
    }
  })
}

const placeholder = computed(() => {
  if (isMobile.value)
    return t('draw.placeholderMobile')
  return t('draw.placeholder')
})

const buttonDisabled = computed(() => {
  return !prompt.value || prompt.value.trim() === '' || drawStore.loading
})

watch(
  prompt,
  (val) => {
    console.log('searchOptionssearchOptions')
    if (val.indexOf('/') === 0 && val.length > 1)
      handleSearchRemote()
  },
)
</script>

<template>
  <NRow class="pb-3">
    <NCol :span="24">
      <div class="flex items-center justify-between space-x-2">
        <NAutoComplete v-model:value="prompt" :options="promptTemplate" :get-show="getShow" :on-select="handleSelect">
          <template #default="{ handleInput, handleBlur, handleFocus, value: slotValue }">
            <NInput
              :value="slotValue" type="textarea" :placeholder="placeholder"
              :autosize="{ minRows: 3, maxRows: isMobile ? 4 : 8 }" @input="handleInput" @focus="handleFocus"
              @blur="handleBlur" @keypress="handleEnter"
            />
          </template>
        </NAutoComplete>
      </div>
    </NCol>
  </NRow>
  <NRow>
    <NCol>
      <NButton type="primary" :disabled="buttonDisabled" @click="handleSubmit">
        {{ t('common.submit') }}
      </NButton>
    </NCol>
  </NRow>
</template>
