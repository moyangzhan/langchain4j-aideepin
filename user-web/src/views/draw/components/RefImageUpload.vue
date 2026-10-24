<script setup lang='ts'>
import { NIcon, NUpload, useMessage } from 'naive-ui'
import type { UploadFileInfo } from 'naive-ui'
import { Add16Filled } from '@vicons/fluent'
import { computed, ref } from 'vue'
import { useAuthStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'

interface RefImage {
  uuid: string
  url: string
}

interface Emit {
  (e: 'change', refImages: RefImage[]): void
}
const emit = defineEmits<Emit>()
const props = defineProps<{ max?: number }>()
const ms = useMessage()
const authStore = useAuthStore()
const refImages = ref<RefImage[]>([])
const fileList = ref<UploadFileInfo[]>([])
// naive-ui 文件 id 与上传结果(uuid/url)的映射,用于删除时定位
// Map from naive-ui file id to upload result (uuid/url), used to locate the entry on removal
const uploadedMap = new Map<string, RefImage>()

const allowedTypes = ['image/png', 'image/jpeg', 'image/webp']

// 达到上限时触发器不再渲染(naive-ui 行为),改为显示一条提示
// At the limit the trigger is no longer rendered (naive-ui behavior); show a hint instead
const atMax = computed(() => refImages.value.length >= (props.max ?? 1))

function notifyChange() {
  emit('change', refImages.value)
}

function clear() {
  fileList.value = []
  uploadedMap.clear()
  refImages.value = []
  notifyChange()
}

defineExpose({ clear })

async function beforeUpload({ file }: { file: UploadFileInfo, fileList: UploadFileInfo[] }) {
  if (!authStore.checkLoginOrShow())
    return false
  if (!file.file?.type || !allowedTypes.includes(file.file.type)) {
    ms.error(t('draw.onlyImageFormat'))
    return false
  }
  return true
}

function handleFinish({ file, event }: { file: UploadFileInfo, event?: ProgressEvent }) {
  const res = JSON.parse((event?.target as XMLHttpRequest).response)
  if (res.success && res.data?.uuid) {
    const entry: RefImage = { uuid: res.data.uuid, url: res.data.url }
    uploadedMap.set(file.id, entry)
    refImages.value.push(entry)
    notifyChange()
  }
  else {
    console.log(`ref image upload err:${res.message}`)
  }
}

function handleRemove({ file }: { file: UploadFileInfo }) {
  const entry = uploadedMap.get(file.id)
  if (entry) {
    api.fileDel(entry.uuid)
    uploadedMap.delete(file.id)
    refImages.value = refImages.value.filter(item => item.uuid !== entry.uuid)
    notifyChange()
  }
}
</script>

<template>
  <NUpload
    v-model:file-list="fileList"
    :class="{ 'ref-upload-full': fileList.length >= (props.max ?? 1) }"
    :action="`/api/image/upload?token=${authStore.token}`"
    :max="props.max ?? 1"
    multiple
    response-type="text"
    list-type="image-card"
    @before-upload="beforeUpload"
    @finish="handleFinish"
    @remove="handleRemove"
  >
    <NIcon size="16">
      <Add16Filled />
    </NIcon>
  </NUpload>
  <!-- 达到参考图数量上限后的提示(触发器已隐藏) / Hint once the reference-image limit is reached (trigger hidden) -->
  <div v-if="atMax" class="pt-1 text-xs text-neutral-400">
    {{ t('draw.refImageMaxReached', { n: props.max ?? 1 }) }}
  </div>
</template>

<style scoped>
/* 达到数量上限后隐藏上传触发卡片(缩略图保留,删除后恢复显示) / Hide the trigger card once the limit is reached (thumbnails kept; reappears after removal) */
.ref-upload-full :deep(.n-upload-trigger) {
  display: none;
}

/* 触发器为紧凑小方形(小于缩略图);已上传的缩略图保持现有尺寸 */
/* Compact square trigger (smaller than thumbnails); uploaded thumbnails keep the current size */
:deep(.n-upload-file-list--grid > .n-upload-trigger) {
  width: 48px;
  height: 48px;
}

:deep(.n-upload-file-list--grid > .n-upload-trigger .n-upload-dragger) {
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0;
  border-radius: 8px;
}
</style>
