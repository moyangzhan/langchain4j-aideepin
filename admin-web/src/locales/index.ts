import type { App } from 'vue'
import { createI18n, useI18n } from 'vue-i18n'
import {
  zhCN as naiveZhCN,
  dateZhCN as naiveDateZhCN,
  enUS as naiveEnUS,
  dateEnUS as naiveDateEnUS,
} from 'naive-ui'
import enUS from './en-US'
import zhCN from './zh-CN'

export type Locale = 'zh-CN' | 'en-US'

const defaultLocale: Locale = 'zh-CN'

const i18n = createI18n({
  locale: defaultLocale,
  fallbackLocale: 'en-US',
  allowComposition: true,
  legacy: false,
  messages: {
    'en-US': enUS,
    'zh-CN': zhCN,
  },
})

export function t(key: string) {
  return i18n.global.t(key)
}

export function setLocale(locale: Locale) {
  i18n.global.locale.value = locale
}

/**
 * Get naive-ui locale and date locale based on current i18n locale.
 * Use this in App.vue's NConfigProvider to sync locale between i18n and naive-ui.
 *
 * Usage in App.vue:
 *   import { getNaiveLocale } from '@/locales'
 *   const naiveLocalePair = computed(() => getNaiveLocale())
 *   <NConfigProvider :locale="naiveLocalePair.locale" :date-locale="naiveLocalePair.dateLocale">
 */
export function getNaiveLocale(locale?: Locale) {
  const currentLocale = locale || (i18n.global.locale.value as Locale)
  switch (currentLocale) {
    case 'en-US':
      return { locale: naiveEnUS, dateLocale: naiveDateEnUS }
    case 'zh-CN':
    default:
      return { locale: naiveZhCN, dateLocale: naiveDateZhCN }
  }
}

export { useI18n }

export function setupI18n(app: App) {
  app.use(i18n)
}

export default i18n
