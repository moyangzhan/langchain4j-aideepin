<script lang="ts" setup>
import { computed, ref } from 'vue'
import { NButton, NInput, NTag, useMessage } from 'naive-ui'
import api from '@/api'
import { t } from '@/locales'

const ms = useMessage()
const resetPasswordReturnType = ref<string>('')
const modifyPasswordMsg = ref<string>('')
const loading = ref(false)
const oldPassword = ref<string>('')
const newPassword = ref<string>('')
const confirmNewPassword = ref<string>('')
const confirmPasswordStatus = computed(() => {
  if (!oldPassword.value || !newPassword.value || !confirmNewPassword.value)
    return undefined
  return newPassword.value !== confirmNewPassword.value ? 'error' : 'success'
})

async function handleModifyPassword() {
  const newPwd = newPassword.value.trim()
  const confirmNewPwd = confirmNewPassword.value.trim()

  if (!newPassword.value || !confirmNewPwd || newPwd !== confirmNewPwd) {
    ms.error(t('common.passwordNotMatch'))
    return
  }

  try {
    loading.value = true
    const result = await api.modifyPassword(oldPassword.value, newPassword.value)
    modifyPasswordMsg.value = result.data as string
    resetPasswordReturnType.value = 'success'
    ms.success(modifyPasswordMsg.value)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
    modifyPasswordMsg.value = error.message as string
    resetPasswordReturnType.value = 'fail'
  } finally {
    loading.value = false
  }
}
</script>

<template>
  <div class="p-4 space-y-5 min-h-[200px]">
    <div class="space-y-6">
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ t('common.password') }}</span>
        <NInput v-model:value="oldPassword" type="password" :placeholder="t('common.password')" show-password-on="click" />
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ t('common.newPassword') }}</span>
        <NInput
          v-model:value="newPassword" type="password" :placeholder="t('common.newPassword')" show-password-on="click"
          :status="confirmPasswordStatus"
        />
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ t('common.newPasswordRepeat') }}</span>
        <NInput
          v-model:value="confirmNewPassword" type="password" :placeholder="t('common.confirmPassword')" show-password-on="click"
          :status="confirmPasswordStatus"
        />
      </div>
      <div v-if="modifyPasswordMsg" class="flex items-center space-x-4">
        <NTag :type="resetPasswordReturnType ? 'success' : 'error'">
          {{ modifyPasswordMsg }}
        </NTag>
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]" />
        <NButton
          type="primary" :disabled="loading || newPassword !== confirmNewPassword" :loading="loading"
          @click="handleModifyPassword"
        >
          {{ t('setting.modifyPassword') }}
        </NButton>
      </div>
    </div>
  </div>
</template>
