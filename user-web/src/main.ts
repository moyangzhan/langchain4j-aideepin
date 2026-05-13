import { createApp } from 'vue'
import masonry from 'vue-next-masonry'
import App from './App.vue'
import { setupI18n } from './locales'
import { setupAssets, setupScrollbarStyle } from './plugins'
import { setupStore } from './store'
import { setupRouter } from './router'

async function bootstrap() {
  const app = createApp(App)
  setupAssets()

  setupScrollbarStyle()

  setupStore(app)

  setupI18n(app)

  masonry.install(app, null)

  await setupRouter(app)

  app.mount('#app')
}

bootstrap()
