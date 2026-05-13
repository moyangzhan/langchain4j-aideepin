<template>
  <n-grid cols="2 s:2 m:2 l:3 xl:3 2xl:3" responsive="screen">
    <n-grid-item>
      <n-form :label-width="80" :model="formValue" :rules="rules" ref="formRef">
        <n-form-item :label="t('system.imageGenerateCount')" path="times">
          <n-input-number v-model:value="formValue.times" :min="0" />
        </n-form-item>

        <n-form-item :label="t('system.timeWindowMinutes')" path="minutes">
          <n-input-number v-model:value="formValue.minutes" :min="0" />
        </n-form-item>

        <div>
          <n-space>
            <n-button type="primary" @click="formSubmit">{{
              t('system.updateImageRateLimit')
            }}</n-button>
          </n-space>
        </div>
      </n-form>
    </n-grid-item>
  </n-grid>
</template>

<script lang="ts" setup>
  import { reactive, watch, ref } from 'vue'
  import { useMessage } from 'naive-ui'
  import { RateLimitConfig } from '/#/sysConfig'
  import api from '@/api/sysConfig.js'
  import { t } from '@/locales'

  const rules = {}
  interface Props {
    ratelimit: RateLimitConfig
  }
  interface Emit {
    (e: 'reloadConfig'): void
  }
  const props = defineProps<Props>()
  const emit = defineEmits<Emit>()
  const formRef: any = ref(null)
  const message = useMessage()
  const formValue = reactive({
    times: 0,
    minutes: 0,
  })
  watch(
    () => props.ratelimit.times,
    () => {
      formValue.times = props.ratelimit.times
      formValue.minutes = props.ratelimit.minutes
    },
    { immediate: true }
  )
  function reloadConfig() {
    emit('reloadConfig')
  }
  function formSubmit() {
    formRef.value.validate(async (errors) => {
      await api.edit({
        name: 'request_image_rate_limit',
        value: JSON.stringify({ times: formValue.times, minutes: formValue.minutes }),
      })
      if (!errors) {
        reloadConfig()
        message.success(t('common.updateSuccess'))
      } else {
        message.error(t('common.fillCompleteInfo'))
      }
    })
  }
</script>
