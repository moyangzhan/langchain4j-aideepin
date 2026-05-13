<template>
  <NConfigProvider
    :locale="naiveLocalePair.locale"
    :theme="getDarkTheme"
    :theme-overrides="getThemeOverrides"
    :date-locale="naiveLocalePair.dateLocale"
  >
    <AppProvider>
      <RouterView />
    </AppProvider>
  </NConfigProvider>
</template>

<script lang="ts" setup>
  import { computed } from 'vue'
  import { darkTheme } from 'naive-ui'
  import { AppProvider } from '@/components/Application'
  import { useDesignSettingStore } from '@/store/modules/designSetting'
  import { lighten } from '@/utils/index'
  import { useLocale } from '@/hooks/useLocale'

  const { naiveLocalePair } = useLocale()

  const designStore = useDesignSettingStore()

  const getThemeOverrides = computed(() => {
    const appTheme = designStore.appTheme
    const lightenStr = lighten(designStore.appTheme, 6)
    return {
      common: {
        primaryColor: appTheme,
        primaryColorHover: lightenStr,
        primaryColorPressed: lightenStr,
        primaryColorSuppl: appTheme,
      },
      LoadingBar: {
        colorLoading: appTheme,
      },
    }
  })

  const getDarkTheme = computed(() => (designStore.darkTheme ? darkTheme : undefined))
</script>
