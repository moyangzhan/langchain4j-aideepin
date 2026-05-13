<template>
  <n-grid cols="2 s:2 m:2 l:3 xl:3 2xl:3" responsive="screen">
    <n-grid-item>
      <n-form :label-width="80" :model="formValue" :rules="rules" ref="formRef">
        <n-form-item :label="t('system.accessKeyId')" path="accessKeyId">
          <n-input v-model:value="formValue.access_key_id" />
        </n-form-item>

        <n-form-item :label="t('system.accessKeySecret')" path="accessKeySecret">
          <n-input v-model:value="formValue.access_key_secret" />
        </n-form-item>

        <n-form-item :label="t('system.bucketName')" path="bucketName">
          <n-input v-model:value="formValue.bucket_name" />
        </n-form-item>

        <n-form-item :label="t('system.endpoint')" path="endpoint">
          <n-input v-model:value="formValue.endpoint" />
        </n-form-item>
        <div>
          <n-space>
            <n-button type="info" :loading="loading" @click="formSubmit">{{
              t('system.saveOssConfig')
            }}</n-button>
          </n-space>
        </div>
      </n-form>
    </n-grid-item>
  </n-grid>
  <br />
  <n-button type="primary" :loading="loading" @click="activeSetting">{{
    t('system.useThisStorage')
  }}</n-button>
</template>

<script lang="ts" setup>
  import { onMounted, ref } from 'vue'
  import { useMessage } from 'naive-ui'
  import { AliyunOssConfig } from '/#/sysConfig'
  import api from '@/api/sysConfig.js'
  import { t } from '@/locales'

  interface Emit {
    (e: 'reload'): void
  }
  const emit = defineEmits<Emit>()
  const loading = ref<boolean>(false)
  const rules = {}
  const formRef: any = ref(null)
  const message = useMessage()
  const formValue = ref<AliyunOssConfig>({
    access_key_id: '',
    access_key_secret: '',
    bucket_name: '',
    endpoint: '',
  })

  async function loadData() {
    const { data: records } = await api.search({ keyword: 'storage_location_ali_oss' }, 1, 10)
    records.forEach((element) => {
      const val = element.value
      if (element.name === 'storage_location_ali_oss') {
        const jsonVal = JSON.parse(val)
        formValue.value = jsonVal
      }
    })
  }
  function formSubmit() {
    console.log(formValue.value)
    console.log(JSON.stringify(formValue.value))
    if (loading.value) return
    loading.value = true
    setTimeout(() => {
      loading.value = false
    }, 2000)
    formRef.value.validate(async (errors) => {
      await api.edit({ name: 'storage_location_ali_oss', value: JSON.stringify(formValue.value) })
      if (!errors) {
        loadData()
        message.success(t('common.updateSuccess'))
      } else {
        message.error(t('common.updateFailed'))
      }
    })
  }
  async function activeSetting() {
    if (loading.value) return
    loading.value = true
    setTimeout(() => {
      loading.value = false
    }, 2000)
    formRef.value.validate(async (errors) => {
      if (!errors) {
        await api.edit({ name: 'storage_location', value: '2' })
        message.success(t('system.aliyunOssSuccess'))
        emit('reload')
      } else {
        message.error(t('system.settingFailed'))
      }
    })
  }

  onMounted(() => {
    loadData()
  })
</script>
