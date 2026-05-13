<script setup lang='ts'>
import { ref } from 'vue'
import { NButton, useMessage, useThemeVars } from 'naive-ui'
import { MediaRecorder, deregister, register } from 'extendable-media-recorder'
import { connect } from 'extendable-media-recorder-wav-encoder'
import { format } from 'date-fns'
import { SvgIcon } from '@/components/common'
import AudioWaveIcon from '@/icons/AudioWave.vue'
import api from '@/api'
import { t } from '@/locales'

const emit = defineEmits<Emit>()
interface Emit {
  (e: 'recorded', audioUrl: string, audioBlob: Blob, audioDuration: number): void
  (e: 'submitted', uuid: string, url: string, audioDuration: number): void
  (e: 'exit'): void
}

const themeVars = useThemeVars()
const audioChunks = ref<Blob[]>([])
const audioUrl = ref('')
const recording = ref(false)
const submitting = ref(false)
const errorMsg = ref('')
const mediaType = 'audio/wav'
const ms = useMessage()
const recordingDuration = ref(0)
let mediaRecorder: any = null
let port: any = null

async function startRecording() {
  errorMsg.value = ''
  if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
    errorMsg.value = t('audio.browserNotSupportRecord')
    return
  }
  console.log('Recording started')
  try {
    recordingDuration.value = 0
    recording.value = true
    recordingDurationCount()
    port = await connect()
    await register(port)
    audioChunks.value = []
    const stream = await navigator.mediaDevices.getUserMedia({ audio: true })
    mediaRecorder = new MediaRecorder(stream, { mimeType: mediaType })
    mediaRecorder.addEventListener('dataavailable', (event: any) => {
      console.log('Audio data available:', event.data)
      if (event.data.size > 0)
        audioChunks.value.push(event.data)
    })
    mediaRecorder.start()
  } catch (exception: any) {
    console.error('startRecording error', exception)
    errorMsg.value = exception.message
    if (exception.message.includes('Permission denied'))
      errorMsg.value = t('audio.getRecordDeviceFailed')
    else if (exception.message.includes('Requested device not found'))
      errorMsg.value = t('audio.noRecordDevice')

    if (!errorMsg.value)
      errorMsg.value = t('audio.getRecordDeviceFailed')
  }
}

function recordingDurationCount() {
  if (!recording.value)
    return
  recordingDuration.value = recordingDuration.value + 200
  setTimeout(recordingDurationCount, 200)
}

async function stopRecording() {
  if (!mediaRecorder)
    return
  mediaRecorder.addEventListener('stop', async () => {
    console.log('Recording stopped', audioChunks)
    const blob = new Blob(audioChunks.value, { type: mediaType })
    audioUrl.value = URL.createObjectURL(blob)
    recording.value = false

    mediaRecorder.removeEventListener('stop', stopRecording)
    mediaRecorder = null
    port = null
    emit('recorded', audioUrl.value, blob, recordingDuration.value / 1000)
  })
  mediaRecorder.stop()
  await deregister(port)
}
async function submit() {
  if (audioChunks.value.length === 0) {
    ms.error(t('audio.noRecordToUpload'))
    return
  }
  if (submitting.value) {
    ms.warning(t('audio.uploadingPleaseWait'))
    return
  }
  try {
    submitting.value = true
    const file = new File([new Blob(audioChunks.value, { type: mediaType })], `record_${format(new Date(), 'yyyyMMddHHmmss')}.wav`, { type: mediaType, lastModified: Date.now() })
    const resp = await api.fileUpload<FileUploaded>(file)
    if (resp.success)
      emit('submitted', resp.data.uuid, resp.data.url, recordingDuration.value / 1000)
    else
      ms.error(t('audio.uploadAudioFailed'))
  } catch (error) {
    ms.error('上传音频失败')
  } finally {
    submitting.value = false
  }
}

function exit() {
  if (submitting.value) {
    ms.warning(t('audio.uploadingPleaseWait'))
    return
  }
  audioUrl.value = ''
  recording.value = false
  audioChunks.value = []
  stopRecording()
  emit('exit')
}

function toggleRecording() {
  if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
    ms.error(t('audio.browserNotSupportRecord'))
    return
  }
  if (submitting.value) {
    ms.warning(t('audio.uploadingPleaseWait'))
    return
  }
  if (recording.value)
    stopRecording()
  else
    startRecording()
}
</script>

<template>
  <div class="flex flex-col items-center space-y-3">
    <SvgIcon
      v-if="!recording" class="text-6xl cursor-pointer custom-hover" icon="pepicons-pop:microphone-circle-filled"
      @click="toggleRecording"
    />
    <AudioWaveIcon v-if="recording" class="cursor-pointer" :placeholder="t('audio.dialogPlaceholder')" @click="toggleRecording" />
    <div v-if="!recording" class="text-sm text-gray-500">
      {{ t('audio.clickToStart') }}
    </div>
    <div v-if="recording" class="text-sm text-gray-500">
      {{ t('audio.dialogCount', { count: recordingDuration / 1000 }) }}
    </div>
    <div v-if="errorMsg" class="text-sm text-red-500">
      {{ errorMsg }}
    </div>
    <div v-if="audioUrl" class="py-2">
      <audio ref="audio" :src="audioUrl" controls />
    </div>
    <div v-if="!recording" class="flex items-center space-x-2 justify-items-end pt-4">
      <NButton ghost class="mt-6" :loading="submitting" :disabled="submitting" @click="exit">
        {{ t('common.cancel') }}
      </NButton>
      <NButton v-if="audioUrl" ghost class="mt-6" :loading="submitting" :disabled="!audioUrl || submitting" @click="submit">
        {{ t('audio.sendLabel') }}
      </NButton>
    </div>
  </div>
</template>

<style lang="less" scoped>
.custom-hover {
  color: v-bind('themeVars.primaryColor');

  :hover {
    color: v-bind('themeVars.primaryColorHover');
  }
}
</style>
