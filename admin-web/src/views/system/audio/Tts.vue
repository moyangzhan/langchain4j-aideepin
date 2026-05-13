<template>
  <div class="m-4">
    <n-alert :title="t('common.remark')" type="info" :bordered="true" closable class="mb-4">
      {{ t('system.ttsExplanation') }}<br />
      1. {{ t('system.ttsExplanation1') }}<br />
      2. {{ t('system.ttsExplanation2') }}
    </n-alert>
    <n-form
      :label-width="150"
      :model="formValue"
      :rules="rules"
      ref="formRef"
      label-placement="left"
    >
      <n-form-item :label="t('system.synthesizerSide')" path="synthesizer_side">
        <n-select :options="synthesizerSide" v-model:value="formValue.synthesizer_side" />
      </n-form-item>
      <n-form-item :label="t('system.model')" path="platform">
        <n-select
          :placeholder="t('system.selectModel')"
          :options="modelOptions"
          :value="formValue.model_name"
          @update:value="handleModelChange"
        />
      </n-form-item>
      <n-form-item :label="t('system.platform')" path="platform">
        <n-select :options="DEFAULT_MODEL_PLATFORMS" v-model:value="formValue.platform" disabled />
      </n-form-item>
    </n-form>
    <n-button type="primary" @click="formSubmit" :loading="submitting" :disable="!submitting">{{
      t('common.update')
    }}</n-button>
  </div>
</template>
<script lang="ts" setup>
  import { ref, reactive, onMounted } from 'vue'
  import { useMessage } from 'naive-ui'
  import { AiModelData } from '/#/aiModel'
  import { getDefaultModelPlatforms, getSynthesizerSide } from '@/utils/constants'
  import api from '@/api/sysConfig.js'
  import modelApi from '@/api/aiModel'
  import { TtsConfig } from '/#/sysConfig'
  import { t } from '@/locales'

  const DEFAULT_MODEL_PLATFORMS = getDefaultModelPlatforms()
  const synthesizerSide = getSynthesizerSide()

  interface modelOption {
    label: string
    value: string
  }
  const rules = {}
  const modelOptions = reactive<modelOption[]>([])
  const models = reactive<AiModelData[]>([])
  const formRef: any = ref(null)
  const formValue = ref<TtsConfig>({
    model_name: '',
    platform: '',
    synthesizer_side: '',
  })
  const message = useMessage()
  const submitting = ref(false)

  async function reloadConfig() {
    const { data } = await api.search({ names: ['tts_setting'] }, 1, 100)
    console.log(data)
    data.records.forEach((record) => {
      formValue.value = JSON.parse(record.value) as TtsConfig
    })

    const { data: modelData } = await modelApi.search({ type: 'tts' }, { current: 1, size: 100 })
    if (modelData.records.length === 0) {
      console.log('can not find asr model')
      return
    }
    modelData.records.forEach((element) => {
      modelOptions.push({ label: element.title, value: element.name })
      models.push({ ...element })
    })
  }

  function handleModelChange(modelName) {
    formValue.value.model_name = modelName
    const md = models.find((model) => model.name === modelName)
    if (!md) {
      message.error('Model not found:' + modelName)
      return
    }
    formValue.value.platform = md.platform
  }

  function formSubmit() {
    if (submitting.value) {
      return
    }
    formRef.value.validate(async (errors) => {
      if (errors) {
        message.error(t('common.fillCompleteInfo'))
      }
      submitting.value = true
      try {
        await api.edit({ name: 'tts_setting', value: JSON.stringify(formValue.value) })
        reloadConfig()
        message.success(t('common.updateSuccess'))
      } catch (error) {
        console.error('Error updating ASR config:', error)
        message.error(t('system.updateFailedRetry'))
      } finally {
        submitting.value = false
      }
    })
  }
  onMounted(async () => {
    reloadConfig()
  })
</script>
