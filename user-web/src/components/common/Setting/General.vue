<script lang="ts" setup>
import { computed, ref } from 'vue'
import { NButton, NImage, NSelect, NSpace } from 'naive-ui'
import type { Language, Theme } from '@/store/modules/app/helper'
import { SvgIcon } from '@/components/common'
import { useAppStore, useAuthStore, useUserStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'
import defaultAvatar from '@/assets/avatar.jpg'

const appStore = useAppStore()
const userStore = useUserStore()
const authStore = useAuthStore()

const theme = computed(() => appStore.theme)

const userInfo = computed(() => userStore.userInfo)

const avatar = ref(userInfo.value.avatar ?? '')

const name = ref(userInfo.value.name ?? '')

const submitting = ref(false)

const language = computed({
  get() {
    return appStore.language
  },
  set(value: Language) {
    appStore.setLanguage(value)
    if (authStore.token)
      api.userEdit({ locale: value } as User.Config)
  },
})

const themeOptions: { label: string; key: Theme; icon: string }[] = [
  {
    label: 'Auto',
    key: 'auto',
    icon: 'ri:contrast-line',
  },
  {
    label: 'Light',
    key: 'light',
    icon: 'ri:sun-foggy-line',
  },
  {
    label: 'Dark',
    key: 'dark',
    icon: 'ri:moon-foggy-line',
  },
]

const languageOptions: { label: string; key: Language; value: Language }[] = [
  { label: '简体中文', key: 'zh-CN', value: 'zh-CN' },
  { label: 'English', key: 'en-US', value: 'en-US' },
]

async function logout() {
  if (submitting.value)
    return
  submitting.value = true
  try {
    await api.logout()
  } catch (error) {
    console.error(error)
  } finally {
    submitting.value = false
  }
  authStore.removeToken()
  userStore.resetUserInfo()
  window.location.reload()
}
</script>

<template>
  <div class="p-4 space-y-5 min-h-[200px]">
    <div class="space-y-6">
      <div class="flex items-center space-x-4">
        <NSpace justify="center" class="w-[100%]">
          <NImage :src="avatar" :fallback-src="defaultAvatar" preview-disabled />
        </NSpace>
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ $t('setting.name') }}</span>
        <div class="w-[200px]">
          {{ name }}
        </div>
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ $t('setting.theme') }}</span>
        <div class="flex flex-wrap items-center gap-4">
          <template v-for="item of themeOptions" :key="item.key">
            <NButton
              size="small" :type="item.key === theme ? 'primary' : undefined"
              @click="appStore.setTheme(item.key)"
            >
              <template #icon>
                <SvgIcon :icon="item.icon" />
              </template>
            </NButton>
          </template>
        </div>
      </div>
      <div class="flex items-center space-x-4">
        <span class="flex-shrink-0 w-[100px]">{{ $t('setting.language') }}</span>
        <div class="flex flex-wrap items-center gap-4">
          <NSelect
            style="width: 140px" :value="language" :options="languageOptions"
            @update-value="(value: Language) => language = value"
          />
        </div>
      </div>
      <div class="flex items-center space-x-4">
        <NButton size="small" type="primary" :loading="submitting" :disabled="submitting" @click="logout">
          {{ t('common.logout') }}
        </NButton>
      </div>
    </div>
  </div>
</template>
