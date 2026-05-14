<script setup lang='ts'>
import { v4 as uuidv4 } from 'uuid'
import { computed, onMounted, ref } from 'vue'
import { NButton, NIcon, NImage, NInput, NModal, NSpace, NTabPane, NTabs, NTag, useMessage } from 'naive-ui'
// import { useRouter } from 'vue-router'
import { CheckmarkCircle } from '@vicons/ionicons5'
import api from '@/api'
import { useAuthStore, useUserStore } from '@/store'
import Icon403 from '@/icons/403.vue'
import { t } from '@/locales'
interface Props {
  visible: boolean
}
interface Emit {
  (e: 'update:visible', visible: boolean): void
}
interface LoginResp {
  token: string
  name: string
  email: string
  uuid: string
  captchaId?: string
}
const props = defineProps<Props>()
const emit = defineEmits<Emit>()

// const router = useRouter()
const authStore = useAuthStore()
const userStore = useUserStore()

const ms = useMessage()
const loading = ref(false)
const email = ref('')
const password = ref('')
const loginCaptchaId = ref('')
const loginCaptchaCode = ref('')
const registerCaptchaId = ref(uuidv4().replace(/-/g, ''))
const registerCaptchaCode = ref('')
const captchaTimestamp = ref(new Date().getTime())
const activeTab = ref('login')
const confirmPassword = ref('')
const disabled = computed(() => !email.value.trim() || loading.value)

const registerReturnSuccess = ref<boolean>(false)
const registerReturnMsg = ref<string>('')

const resetPasswordReturnMsg = ref<string>('')

const show = computed({
  get: () => props.visible,
  set: (visible: boolean) => emit('update:visible', visible),
})

onMounted(async () => {
  console.info('permission,onmounted')
  if (!authStore.token) {
    activeTab.value = 'login'
    show.value = true
  }
})

function handlePress(event: KeyboardEvent) {
  if (event.key === 'Enter' && !event.shiftKey) {
    event.preventDefault()
    handleLogin()
  }
}

const confirmPasswordStatus = computed(() => {
  if (!password.value || !confirmPassword.value)
    return undefined
  return password.value !== confirmPassword.value ? 'error' : 'success'
})

async function handleLogin() {
  const name = email.value.trim()
  const pwd = password.value.trim()
  if (!name || !pwd) {
    ms.error(t('common.emailOrPasswordEmpty'))
    return
  }

  if (loginCaptchaId.value && !loginCaptchaCode.value) {
    ms.error(t('common.pleaseInputCaptcha'))
    return
  }
  try {
    loading.value = true
    const result = await api.login<LoginResp>(email.value, pwd, loginCaptchaId.value, loginCaptchaCode.value)
    await authStore.setToken(result.data.token)
    await userStore.updateUserInfo(result.data)
    ms.success('success')
    // router.go(0)
    loading.value = false
  } catch (error: any) {
    console.error('login error', error)
    ms.error(error.message ?? 'error')
    if (error.data?.captchaId) {
      // 显示验证码
      loginCaptchaId.value = error.data.captchaId
    }
    password.value = ''
  } finally {
    loading.value = false
  }
}

async function handleRegister() {
  const mail = email.value.trim()
  const pwd = password.value.trim()
  const confirmPwd = confirmPassword.value.trim()

  if (!mail || !pwd || !confirmPwd || pwd !== confirmPwd) {
    ms.error(t('common.passwordNotMatch'))
    return
  }

  try {
    loading.value = true
    const result = await api.register(mail, pwd, registerCaptchaId.value, registerCaptchaCode.value)
    registerReturnSuccess.value = true
    registerReturnMsg.value = result.data as string
    ms.success(registerReturnMsg.value)
  } catch (error: any) {
    ms.error(error.message ?? 'error')
    registerReturnMsg.value = error.message ?? 'error'
  } finally {
    loading.value = false
  }
}

async function handleForgotPassword() {
  const name = email.value.trim()

  if (!name)
    return

  try {
    loading.value = true
    const result = await api.passwordFind(name)
    ms.success(result.data as string)
    resetPasswordReturnMsg.value = result.data as string
  } catch (error: any) {
    ms.error(error.message ?? 'error')
  } finally {
    loading.value = false
  }
}
</script>

<template>
  <NModal :show="visible" style="width: 90%; max-width: 640px">
    <div class="p-10 bg-white rounded dark:bg-slate-800">
      <div class="space-y-4">
        <header class="space-y-2">
          <!-- <p class="text-base text-center text-slate-500 dark:text-slate-500">
            {{ t('common.unauthorizedTips') }}
          </p> -->
          <Icon403 class="w-[200px] m-auto" />
        </header>

        <NTabs v-model:value="activeTab" default-value="login" type="line">
          <NTabPane name="login" :tab="t('common.login')">
            <NSpace vertical>
              <NInput v-model:value="email" type="text" :placeholder="t('common.email')" :input-props="{ autocomplete: 'on' }" />
              <NInput
                v-model:value="password" type="password" show-password-on="click"
                :placeholder="t('common.password')" @keypress="handlePress"
              />
              <NSpace :wrap-item="false">
                <NInput
                  v-if="loginCaptchaId" v-model:value="loginCaptchaCode" style="flex:1;height:40px;"
                  :placeholder="t('common.captcha')"
                />
                <NImage
                  v-if="loginCaptchaId"
                  :src="`/api/auth/login/captcha?captchaId=${loginCaptchaId}&t_${captchaTimestamp}`"
                  @click="captchaTimestamp = new Date().getTime()"
                />
              </NSpace>
              <NSpace justify="space-between">
                <NButton type="primary" size="medium" :disabled="disabled" :loading="loading" @click="handleLogin">
                  {{ t('common.login') }}
                </NButton>
                <NButton text type="primary" @click="activeTab = 'forgotPassword'">
                  {{ t('common.forgotPassword') }}
                </NButton>
              </NSpace>
            </NSpace>
          </NTabPane>

          <NTabPane name="register" :tab="t('common.register')">
            <NSpace vertical>
              <NInput v-model:value="email" type="text" size="large" :placeholder="t('common.email')" />
              <NInput
                v-model:value="password" type="password" size="large" show-password-on="click"
                :placeholder="t('common.password')"
              />
              <NInput
                v-model:value="confirmPassword" type="password" size="large" show-password-on="click"
                :placeholder="t('common.confirmPassword')" :status="confirmPasswordStatus"
              />
              <NSpace :wrap-item="false">
                <NInput
                  v-if="registerCaptchaId" v-model:value="registerCaptchaCode" size="large"
                  style="flex:1;height:40px;" :placeholder="t('common.captcha')"
                />
                <NImage
                  v-if="registerCaptchaId" object-fit="fill"
                  :src="`/api/auth/register/captcha?captchaId=${registerCaptchaId}&_t=${captchaTimestamp}`"
                  @click="captchaTimestamp = new Date().getTime()"
                />
              </NSpace>
              <NSpace v-if="registerReturnMsg">
                <NTag :type="registerReturnSuccess ? 'success' : 'error'">
                  {{ registerReturnMsg }}
                  <template #icon>
                    <NIcon :component="CheckmarkCircle" />
                  </template>
                </NTag>
              </NSpace>
              <NButton
                type="primary" size="medium" :disabled="disabled || password !== confirmPassword"
                :loading="loading" @click="handleRegister"
              >
                {{ t('common.register') }}
              </NButton>
            </NSpace>
          </NTabPane>

          <NTabPane name="forgotPassword" :tab="t('common.findMyPassword')">
            <NSpace vertical>
              <NInput v-model:value="email" type="text" size="large" :placeholder="t('common.email')" />
              <NTag v-if="resetPasswordReturnMsg" :type="resetPasswordReturnMsg ? 'success' : 'error'">
                {{ resetPasswordReturnMsg }}
                <template #icon>
                  <NIcon :component="CheckmarkCircle" />
                </template>
              </NTag>
              <NButton type="primary" :disabled="email.length <= 0" :loading="loading" @click="handleForgotPassword">
                {{ t('common.resetPassword') }}
              </NButton>
            </NSpace>
          </NTabPane>
        </NTabs>
      </div>
    </div>
  </NModal>
</template>
