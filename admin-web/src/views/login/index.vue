<template>
  <div class="view-account">
    <div class="view-account-header"></div>
    <div class="view-account-container">
      <div class="view-account-top">
        <div class="view-account-top-logo">
          <img :src="websiteConfig.loginImage" alt="" />
        </div>
        <div class="view-account-top-desc">{{ websiteConfig.loginDesc }}</div>
      </div>
      <div class="view-account-form">
        <n-form
          ref="formRef"
          label-placement="left"
          size="large"
          :model="formInline"
          :rules="rules"
        >
          <n-form-item path="username">
            <n-input v-model:value="formInline.email" :placeholder="t('login.emailPlaceholder')">
              <template #prefix>
                <n-icon size="18" color="#808695">
                  <PersonOutline />
                </n-icon>
              </template>
            </n-input>
          </n-form-item>
          <n-form-item path="password">
            <n-input
              v-model:value="formInline.password"
              type="password"
              showPasswordOn="click"
              :placeholder="t('login.passwordPlaceholder')"
            >
              <template #prefix>
                <n-icon size="18" color="#808695">
                  <LockClosedOutline />
                </n-icon>
              </template>
            </n-input>
          </n-form-item>
          <n-form-item v-if="loginCaptchaId">
            <n-input
              v-model:value="loginCaptchaCode"
              style="flex: 1; height: 40px"
              :placeholder="t('login.captchaPlaceholder')"
            />
            <n-image
              :src="`/api/auth/login/captcha?captchaId=${loginCaptchaId}&t_${captchaTimestamp}`"
              @click="captchaTimestamp = new Date().getTime()"
            />
          </n-form-item>
          <!-- <n-form-item class="default-color">
            <div class="flex justify-between">
              <div class="flex-initial">
                <n-checkbox v-model:checked="autoLogin">自动登录</n-checkbox>
              </div>
              <div class="flex-initial order-last">
                <a href="javascript:">忘记密码</a>
              </div>
            </div>
          </n-form-item> -->
          <n-form-item>
            <n-button type="primary" @click="handleSubmit" size="large" :loading="loading" block>
              {{ t('login.title') }}
            </n-button>
          </n-form-item>
          <n-form-item class="default-color">
            <div class="flex-initial" style="margin-left: auto">
              <a href="http://www.aideepin.com" target="_blank">{{ t('login.goToSite') }}</a>
            </div>
          </n-form-item>
        </n-form>
      </div>
    </div>
  </div>
</template>

<script lang="ts" setup>
  import { reactive, ref } from 'vue'
  import { useRoute, useRouter } from 'vue-router'
  import { useUserStore } from '@/store/modules/user'
  import { useMessage } from 'naive-ui'
  import { t } from '@/locales'
  import { PersonOutline, LockClosedOutline } from '@vicons/ionicons5'
  import { PageEnum } from '@/enums/pageEnum'
  import { websiteConfig } from '@/config/website.config'
  interface FormState {
    email: string
    password: string
    captchaId?: string
    captchaCode?: string
  }

  const loginCaptchaId = ref('')
  const loginCaptchaCode = ref('')
  const captchaTimestamp = ref(new Date().getTime())
  const formRef = ref()
  const message = useMessage()
  const loading = ref(false)
  // const autoLogin = ref(false);
  const LOGIN_NAME = PageEnum.BASE_LOGIN_NAME

  const formInline = reactive({
    email: '',
    password: '',
    captchaId: '',
    captchaCode: '',
    isCaptcha: true,
  })

  const rules = {
    email: { required: true, message: () => t('login.emailRequired'), trigger: 'blur' },
    password: { required: true, message: () => t('login.passwordRequired'), trigger: 'blur' },
  }

  const userStore = useUserStore()

  const router = useRouter()
  const route = useRoute()

  const handleSubmit = (e) => {
    e.preventDefault()
    if (loginCaptchaId.value && !loginCaptchaCode.value) {
      message.error(t('login.captchaRequired'))
      return
    }
    formRef.value.validate(async (errors) => {
      if (!errors) {
        const { email, password } = formInline
        message.loading(t('login.loggingIn'))
        loading.value = true
        const params: FormState = {
          email,
          password,
          captchaId: loginCaptchaId.value,
          captchaCode: loginCaptchaCode.value,
        }
        try {
          const { success, message: msg, data } = await userStore.login(params)
          message.destroyAll()
          if (success) {
            const toPath = decodeURIComponent((route.query?.redirect || '/') as string)
            message.success(t('login.loginSuccess'))
            if (route.name === LOGIN_NAME) {
              router.replace('/')
            } else router.replace(toPath)
          } else if (data?.captchaId) {
            // 显示验证码
            loginCaptchaId.value = data.captchaId
          } else {
            message.error(msg || t('login.loginFailed'))
          }
        } finally {
          loading.value = false
        }
      } else {
        message.error(t('login.fillCompleteInfo'))
      }
    })
  }
</script>

<style lang="less" scoped>
  .view-account {
    display: flex;
    flex-direction: column;
    height: 100vh;
    overflow: auto;

    &-container {
      flex: 1;
      padding: 32px 12px;
      max-width: 384px;
      min-width: 320px;
      margin: 0 auto;
    }

    &-top {
      padding: 32px 0;
      text-align: center;

      &-desc {
        font-size: 14px;
        color: #808695;
      }
    }

    &-other {
      width: 100%;
    }

    .default-color {
      color: #515a6e;

      .ant-checkbox-wrapper {
        color: #515a6e;
      }
    }
  }

  @media (min-width: 768px) {
    .view-account {
      background-image: url('../../assets/images/login.svg');
      background-repeat: no-repeat;
      background-position: 50%;
      background-size: 100%;
    }

    .page-account-container {
      padding: 32px 0 24px 0;
    }
  }
</style>
