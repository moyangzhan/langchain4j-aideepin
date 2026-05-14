import type { App } from 'vue'
import { createI18n } from 'vue-i18n'
import enUS from './en-US'
import zhCN from './zh-CN'
import type { Language } from '@/store/modules/app/helper'

const defaultLocale = 'zh-CN'

const i18n = createI18n({
  legacy: false,
  locale: defaultLocale,
  fallbackLocale: 'en-US',
  messages: {
    'en-US': enUS,
    'zh-CN': zhCN,
  },
})

export const t = i18n.global.t

export function setLocale(locale: Language) {
  i18n.global.locale.value = locale
}

export function setupI18n(app: App) {
  app.use(i18n)
}

export default i18n
