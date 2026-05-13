<template>
  <n-grid cols="2 s:2 m:2 l:3 xl:3 2xl:3" responsive="screen">
    <n-grid-item>
      <n-form :label-width="80" :model="formValue" :rules="rules" ref="formRef">
        <n-form-item :label="t('system.dailyMaxConsume')" path="daily">
          <n-input-number v-model:value="formValue.daily" :min="0" />
        </n-form-item>

        <n-form-item :label="t('system.monthlyMaxConsume')" path="monthly">
          <n-input-number v-model:value="formValue.monthly" :min="0" />
        </n-form-item>

        <div>
          <n-space>
            <n-button type="primary" @click="formSubmit">{{
              t('system.updateTokenLimit')
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
  import { QuotaConfig } from '/#/sysConfig'
  import api from '@/api/sysConfig.js'
  import { t } from '@/locales'

  const rules = {}
  interface Props {
    quota: QuotaConfig
  }
  interface Emit {
    (e: 'reloadConfig'): void
  }
  const props = defineProps<Props>()
  const emit = defineEmits<Emit>()
  const formRef: any = ref(null)
  const message = useMessage()
  const formValue = reactive({
    daily: 0,
    monthly: 0,
  })
  watch(
    () => props.quota.daily,
    () => {
      formValue.daily = props.quota.daily
      formValue.monthly = props.quota.monthly
    },
    { immediate: true }
  )
  function reloadConfig() {
    emit('reloadConfig')
  }
  function formSubmit() {
    console.log(props.quota)
    console.log(formValue)
    formRef.value.validate(async (errors) => {
      await api.edit({ name: 'quota_by_token_daily', value: formValue.daily })
      await api.edit({ name: 'quota_by_token_monthly', value: formValue.monthly })
      if (!errors) {
        reloadConfig()
        message.success(t('common.updateSuccess'))
      } else {
        message.error(t('common.fillCompleteInfo'))
      }
    })
  }
</script>
