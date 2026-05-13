<script setup lang='ts'>
import { ref } from 'vue'
import { AnimalCat24Regular } from '@vicons/fluent'
import { NButton, NCol, NFlex, NIcon, NIconWrapper, NInputNumber, NRadio, NRadioGroup, NRow, NSlider, NSpace, useMessage } from 'naive-ui'
import SearchInput from '@/views/draw/components/SearchInput.vue'
import { checkProcess } from '@/views/draw/helper'
import { useAppStore, useDrawStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { emptyDraw } from '@/utils/functions'

interface Emit {
  (e: 'submitted'): void
}
const emit = defineEmits<Emit>()
const drawStore = useDrawStore()
const appStore = useAppStore()
const ms = useMessage()
const imageSizes = ['1024*1024', '720*1280', '1280*720']
const selectedImageSize = ref<string>('1024*1024')
const generateImageNumber = ref<number>(1)
const randomSeed = ref<number>(-1)

function sizeChange(value: string) {
  selectedImageSize.value = value
}
function imageNumberChange(value: number) {
  generateImageNumber.value = value
}

async function handleSubmit(prompt: string) {
  console.log(`GenerateImage submit:${prompt}`)
  try {
    const resp = await api.imageGenerate<CreateImageResult>({ interactingMethod: 1, modelName: appStore.selectedImageModel.modelName, prompt, size: selectedImageSize.value, number: generateImageNumber.value, seed: randomSeed.value })
    const uuid = resp.data.uuid
    drawStore.setLoadingUuid(uuid)

    const draw = emptyDraw()
    draw.uuid = uuid
    draw.prompt = prompt
    draw.aiModelName = appStore.selectedImageModel.modelName
    drawStore.pushOne(draw)

    emit('submitted')

    setTimeout(() => {
      console.log(`checkProcess:${uuid}`)
      checkProcess(uuid)
    }, 5000)
  } catch (error: any) {
    const e = error as { message: string }
    ms.error(e.message)
  }
}
</script>

<template>
  <div>
    <NRow class="pt-4 pb-4">
      <NCol :span="2" class="min-w-fit">
        {{ t('common.imageSize') }}
      </NCol>
      <NCol :span="12">
        <NRadioGroup :value="selectedImageSize" name="radiogroup" :on-update:value="sizeChange">
          <NSpace>
            <NRadio v-for="imageSize in imageSizes" :key="imageSize" :value="imageSize">
              {{ imageSize }}
            </NRadio>
          </NSpace>
        </NRadioGroup>
      </NCol>
    </NRow>
    <NRow class="pb-4">
      <NCol :span="2" class="min-w-fit">
        {{ t('common.imageCount') }}
      </NCol>
      <NCol :span="12">
        <NSlider :value="generateImageNumber" :step="1" :min="1" :max="4" :on-update:value="imageNumberChange">
          <template #thumb>
            <NIconWrapper :size="24" :border-radius="12">
              <NIcon :size="18" :component="AnimalCat24Regular" />
            </NIconWrapper>
          </template>
        </NSlider>
        <span>{{ generateImageNumber }}{{ t('draw.imageUnit') }}</span>
      </NCol>
    </NRow>
    <NRow class="pb-4">
      <NCol :span="2" class="min-w-fit">
        {{ t('common.randomSeed') }}
      </NCol>
      <NCol :span="12">
        <NFlex align="center">
          <NInputNumber v-model:value="randomSeed" :min="-1" :max="2147483647" class="grow" />
          <NButton type="primary" size="tiny" ghost @click="randomSeed = Math.floor(Math.random() * 2147483647)">
            {{ t('common.randomGenerate') }}
          </NButton>
        </NFlex>
      </NCol>
    </NRow>
    <SearchInput @submit="handleSubmit" />
  </div>
</template>
