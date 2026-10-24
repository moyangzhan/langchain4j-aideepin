<script setup lang='ts'>
import { computed, ref, watch } from 'vue'
import { AnimalCat24Regular, QuestionCircle16Regular } from '@vicons/fluent'
import { NButton, NCol, NFlex, NIcon, NIconWrapper, NInputNumber, NRadio, NRadioGroup, NRow, NSlider, NSpace, NTooltip, useMessage } from 'naive-ui'
import SearchInput from '@/views/draw/components/SearchInput.vue'
import RefImageUpload from '@/views/draw/components/RefImageUpload.vue'
import ParamsPanel from '@/views/draw/components/ParamsPanel.vue'
import { checkProcess } from '@/views/draw/helper'
import { useAppStore, useDrawStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { emptyDraw } from '@/utils/functions'
interface Emit {
  (e: 'submitted'): void
}
interface RefImage {
  uuid: string
  url: string
}
interface SizeOption { value: string, label: string }
const emit = defineEmits<Emit>()
const drawStore = useDrawStore()
const appStore = useAppStore()
const ms = useMessage()

// 表单项全部由模型配置(adi_ai_model.properties,统一五键结构,见 typings 的 ImageModelProperties)驱动;
// 计算属性保留兜底(空数组/false 时隐藏对应表单项),防管理员自配模型缺键
// Form fields are all model-config driven (adi_ai_model.properties, uniform five-key shape,
// see ImageModelProperties in typings); computeds keep fallbacks (empty/false hides the
// field), guarding against admin-configured models missing keys
const modelProps = computed(() => {
  return appStore.selectedImageModel.properties as ImageModelProperties | undefined
})

// 尺寸选项来自选中模型的 properties.sizes(DB 配置驱动),未配置时回退默认
// Size options come from the selected model's properties.sizes (DB-driven); falls back if unset
const imageSizes = computed<SizeOption[]>(() => {
  return modelProps.value?.sizes?.length
    ? modelProps.value.sizes
    : [{ value: '1024*1024', label: '1024*1024' }]
})
const selectedImageSize = ref<string>('')
watch(() => imageSizes.value, (sizes) => {
  if (sizes.length && !sizes.some(s => s.value === selectedImageSize.value))
    selectedImageSize.value = sizes[0].value
}, { immediate: true })

// 数量:模型支持多张(max_images > 1)时展示滑杆
// Count: show the slider when the model supports multiple images (max_images > 1)
const maxImages = computed(() => modelProps.value?.max_images || 1)
const generateImageNumber = ref<number>(1)
watch(maxImages, (max) => {
  if (generateImageNumber.value > max)
    generateImageNumber.value = max
}, { immediate: true })

// 质量档位:模型配置了 qualities 时展示
// Quality: shown when the model declares qualities
const qualities = computed<string[]>(() => modelProps.value?.qualities || [])
const selectedQuality = ref<string>('')
watch(qualities, (list) => {
  if (list.length && !list.includes(selectedQuality.value))
    selectedQuality.value = list[0]
}, { immediate: true })

// 种子:模型声明 is_support_seed 时展示
// Seed: shown when the model declares is_support_seed
const supportsSeed = computed(() => modelProps.value?.is_support_seed ?? false)
const randomSeed = ref<number>(-1)

// 图生图:仅声明了图片输入(input_types 含 image)的模型展示参考图上传
// Image-to-image: show the reference-image upload only for models declaring image input (input_types contains image)
const supportsI2i = computed(() => {
  return appStore.selectedImageModel.inputTypes?.split(',').includes('image') ?? false
})
const maxRefImages = computed(() => modelProps.value?.max_ref_images || 1)
const refImages = ref<RefImage[]>([])
// 切换模型时重置参考图(不同模型能力与上限不同)
// Reset reference images on model switch (capabilities and limits differ per model)
watch(() => appStore.selectedImageModel, () => {
  refImages.value = []
})
const refUploadRef = ref<InstanceType<typeof RefImageUpload>>()

function sizeChange(value: string) {
  selectedImageSize.value = value
}
function imageNumberChange(value: number) {
  generateImageNumber.value = value
}
function onRefImagesChange(images: RefImage[]) {
  refImages.value = images
}

async function handleSubmit(prompt: string) {
  console.log(`GenerateImage submit:${prompt}`)
  try {
    const dynamicParams = refImages.value.length
      ? {
              ref_image_uuids: refImages.value.map(item => item.uuid),
              ref_image_urls: refImages.value.map(item => item.url),
          }
      : undefined
    const resp = await api.imageGenerate<CreateImageResult>({
      interactingMethod: 1,
      modelName: appStore.selectedImageModel.modelName,
      prompt,
      size: selectedImageSize.value,
      number: generateImageNumber.value,
      seed: supportsSeed.value ? randomSeed.value : undefined,
      quality: qualities.value.length ? selectedQuality.value : undefined,
      dynamicParams,
    })
    const uuid = resp.data.uuid
    drawStore.setLoadingUuid(uuid)
    // 提交成功后清空表单参考图(与提示词一致);失败路径不清,保留重试
    // Clear the form's reference images after a successful submit (like the prompt); keep them on failure for retry
    refUploadRef.value?.clear()

    const draw = emptyDraw()
    draw.uuid = uuid
    draw.prompt = prompt
    draw.aiModelName = appStore.selectedImageModel.modelName
    if (dynamicParams)
      draw.dynamicParams = dynamicParams
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
    <ParamsPanel>
      <NRow class="pt-2 pb-4">
        <NCol :span="2" class="min-w-fit">
          {{ t('common.imageSize') }}
        </NCol>
        <NCol :span="12">
          <NRadioGroup :value="selectedImageSize" name="radiogroup" :on-update:value="sizeChange">
            <NSpace>
              <NRadio v-for="imageSize in imageSizes" :key="imageSize.value" :value="imageSize.value">
                {{ imageSize.label }}
              </NRadio>
            </NSpace>
          </NRadioGroup>
        </NCol>
      </NRow>
      <NRow v-if="maxImages > 1" class="pb-4">
        <NCol :span="2" class="min-w-fit">
          {{ t('common.imageCount') }}
        </NCol>
        <NCol :span="12">
          <NSlider :value="generateImageNumber" :step="1" :min="1" :max="maxImages" :on-update:value="imageNumberChange">
            <template #thumb>
              <NIconWrapper :size="24" :border-radius="12">
                <NIcon :size="18" :component="AnimalCat24Regular" />
              </NIconWrapper>
            </template>
          </NSlider>
          <span>{{ generateImageNumber }}{{ t('draw.imageUnit') }}</span>
        </NCol>
      </NRow>
      <NRow v-if="qualities.length" class="pb-4">
        <NCol :span="2" class="min-w-fit">
          {{ t('common.imageQuality') }}
        </NCol>
        <NCol :span="12">
          <NRadioGroup v-model:value="selectedQuality" name="qualitygroup">
            <NSpace>
              <NRadio v-for="quality in qualities" :key="quality" :value="quality">
                {{ quality }}
              </NRadio>
            </NSpace>
          </NRadioGroup>
        </NCol>
      </NRow>
      <NRow v-if="supportsSeed" class="pb-4">
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
      <NRow v-if="supportsI2i" class="pb-4">
        <NCol :span="2" class="min-w-fit">
          {{ t('draw.refImage') }}
          <NTooltip trigger="hover">
            <template #trigger>
              <NIcon style="padding-top: 0.1rem">
                <QuestionCircle16Regular />
              </NIcon>
            </template>
            <span>{{ t('draw.refImageTip') }}</span>
          </NTooltip>
        </NCol>
        <NCol :span="12">
          <RefImageUpload ref="refUploadRef" :key="appStore.selectedImageModel.modelName" :max="maxRefImages" @change="onRefImagesChange" />
        </NCol>
      </NRow>
    </ParamsPanel>
    <SearchInput @submit="handleSubmit" />
  </div>
</template>
