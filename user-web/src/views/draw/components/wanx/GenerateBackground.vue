<script setup lang='ts'>
import { ref } from 'vue'
import { NAlert, NCard, NCol, NCollapseTransition, NIcon, NRow, NSpace, NSwitch, NTooltip, NUpload, useMessage } from 'naive-ui'
import type { UploadFileInfo } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import SearchInput from '@/views/draw/components/SearchInput.vue'
import { checkProcess } from '@/views/draw/helper'
import { useAppStore, useAuthStore, useDrawStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'
import { emptyDraw } from '@/utils/functions'
interface Emit {
  (e: 'submitted'): void
}
const emit = defineEmits<Emit>()
const ms = useMessage()
const drawStore = useDrawStore()
const authStore = useAuthStore()
const appStore = useAppStore()
const showTip = ref<boolean>(false)
const selectedImageSize = ref<string>('')
const baseImageList = ref<UploadFileInfo[]>()
const baseImage = ref<UploadResult>({ uuid: '', url: '' })
const refImageList = ref<UploadFileInfo[]>()
const refImage = ref<UploadResult>({ uuid: '', url: '' })
const isProd = import.meta.env.PROD
interface UploadResult {
  uuid: string
  url: string
}

async function beforeUpload(data: { file: UploadFileInfo; fileList: UploadFileInfo[] }) {
  if (!authStore.checkLoginOrShow())
    return false
  if (data.file.file?.type !== 'image/png') {
    ms.error(t('draw.onlyPngFormat'))
    return false
  }
  return true
}

async function beforeUpload2(data: { file: UploadFileInfo; fileList: UploadFileInfo[] }) {
  if (!authStore.checkLoginOrShow())
    return false
  return true
}

// Upload original image finish
function handleBaseImageFinish({ file, event }: { file: UploadFileInfo; event?: ProgressEvent }) {
  const res = JSON.parse((event?.target as XMLHttpRequest).response)
  if (res.success) {
    baseImage.value = res.data
    baseImageList.value?.push(file)
    console.log(`image url:${baseImage.value}`)
  } else {
    console.log(`handleBaseImageFinish err:${res.data}`)
  }
}

function handleRefImageFinish({ file, event }: { file: UploadFileInfo; event?: ProgressEvent }) {
  const res = JSON.parse((event?.target as XMLHttpRequest).response)
  if (res.success) {
    refImage.value = res.data
    refImageList.value?.push(file)
    console.log(`ref image uuid:${refImage.value}`)
  } else {
    console.log(`handleRefImageFinish err:${res.data}`)
  }
}

// Delete original image
function removeBaseImage({ file }: { file: UploadFileInfo }) {
  if (baseImage.value.uuid)
    api.fileDel(baseImage.value.uuid)
  baseImage.value.url = ''
  baseImage.value.uuid = ''
}

function removeRefImage({ file }: { file: UploadFileInfo }) {
  if (refImage.value.uuid)
    api.fileDel(refImage.value.uuid)
  refImage.value.uuid = ''
  refImage.value.url = ''
}

async function handleSubmit(prompt: string) {
  if (!baseImage.value.url) {
    ms.error(t('draw.pleaseUploadMainImage'))
    return
  }
  if (!refImage.value.url && !prompt) {
    ms.error(t('draw.pleaseUploadGuideOrPrompt'))
    return
  }
  try {
    const dynamicParams = {
      base_image_url: baseImage.value.url,
      ref_image_url: refImage.value.url,
      ref_prompt: prompt,
    }
    const params = {
      interactingMethod: 4,
      modelName: appStore.selectedImageModel.modelName,
      prompt,
      size: selectedImageSize.value,
      number: 1,
      seed: -1,
      dynamicParams,
    }
    const resp = await api.imageGenerate<CreateImageResult>(params)
    const uuid = resp.data.uuid

    const draw = emptyDraw()
    draw.uuid = uuid
    draw.prompt = prompt
    draw.interactingMethod = 4
    draw.aiModelName = appStore.selectedImageModel.modelName
    draw.dynamicParams = dynamicParams
    drawStore.setLoadingUuid(uuid)
    drawStore.pushOne(draw)

    emit('submitted')
    setTimeout(() => {
      checkProcess(uuid)
    }, 3000)
  } catch (error) {
    const e = error as { message: string }
    ms.error(e.message)
  }
}
</script>

<template>
  <div>
    <NAlert v-if="!isProd" :title="t('common.tip')" type="error">
      {{ t('draw.ossNote1') }}<br>
      {{ t('draw.ossNote2') }}<br>
      {{ t('draw.ossNote3') }}<br>
      {{ t('draw.ossReason') }}
    </NAlert>
    <NSpace vertical>
      <NSwitch v-model:value="showTip">
        <template #checked>
          {{ t('draw.usageNote') }}
        </template>
        <template #unchecked>
          {{ t('draw.usageNote') }}
        </template>
      </NSwitch>
      <NCollapseTransition :show="showTip">
        <NCard :bordered="true" embedded>
          {{ t('draw.mainImageDesc') }}<br>
          {{ t('draw.guideImageDescShort') }}<br>
          {{ t('draw.promptDesc') }}<br>
          {{ t('draw.guideOrPromptRequired') }}
        </NCard>
      </NCollapseTransition>
    </NSpace>
    <NRow>
      <NCol :span="2" class="min-w-fit">
        {{ t('draw.mainImage') }}<span class="text-red-500">*</span>
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <span>
            {{ t('draw.backgroundTransparent') }}
          </span>
        </NTooltip>
      </NCol>
      <NCol :span="12">
        <NUpload
          :action="`/api/image/upload?token=${authStore.token}`" :max="1" response-type="text"
          list-type="image-card" :default-file-list="baseImageList" @before-upload="beforeUpload"
          @finish="handleBaseImageFinish" @remove="removeBaseImage"
        >
          {{ t('draw.pngLimit') }}
        </NUpload>
      </NCol>
    </NRow>
    <NRow>
      <NCol :span="2" class="min-w-fit">
        {{ t('draw.guideImageLabel') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <span>
            {{ t('draw.imageRequirement') }}<br>{{ t('draw.guideImageDesc') }}
          </span>
        </NTooltip>
      </NCol>
      <NCol :span="12">
        <NUpload
          :action="`/api/image/upload?token=${authStore.token}`" :max="1" response-type="text"
          list-type="image-card" :default-file-list="refImageList" @before-upload="beforeUpload2"
          @finish="handleRefImageFinish" @remove="removeRefImage"
        >
          {{ t('draw.commonImageFormat') }}
        </NUpload>
      </NCol>
    </NRow>
    <SearchInput @submit="handleSubmit" />
  </div>
</template>
