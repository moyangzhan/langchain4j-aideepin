import { computed, ref, watch } from 'vue'
import { setLocale, getNaiveLocale } from '@/locales'
import type { Locale } from '@/locales'

const STORAGE_KEY = 'admin-locale'

const currentLocale = ref<Locale>((localStorage.getItem(STORAGE_KEY) as Locale) || 'zh-CN')

watch(
  currentLocale,
  (val) => {
    setLocale(val)
  },
  { immediate: true }
)

export function useLocale() {
  const naiveLocalePair = computed(() => getNaiveLocale(currentLocale.value))

  function changeLocale(locale: Locale) {
    currentLocale.value = locale
    localStorage.setItem(STORAGE_KEY, locale)
  }

  return {
    locale: currentLocale,
    naiveLocalePair,
    changeLocale,
  }
}
