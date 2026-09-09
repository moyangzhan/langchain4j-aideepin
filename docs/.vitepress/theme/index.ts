import type { Theme } from 'vitepress'
import DefaultTheme from 'vitepress/theme'
import { defineComponent, h, nextTick, onMounted, watch } from 'vue'
import { useRoute } from 'vitepress'
import mediumZoom from 'medium-zoom'
import 'medium-zoom/dist/style.css'
import './custom.css'

const initZoom = () => {
  mediumZoom('.main figure img', { background: 'var(--vp-c-bg)' })
}

const Layout = defineComponent(() => {
  const route = useRoute()
  onMounted(() => initZoom())
  watch(() => route.path, () => nextTick(() => initZoom()))
  return () => h(DefaultTheme.Layout)
})

export default {
  extends: DefaultTheme,
  Layout,
} satisfies Theme
