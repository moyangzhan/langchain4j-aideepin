<script setup lang='ts'>
import { ref } from 'vue'
import { NTab, NTabPane, NTabs } from 'naive-ui'
import GenerateImage from './GenerateImage.vue'
import { t } from '@/locales'

interface Emit {
  (e: 'submitted'): void
}
interface TabObj {
  name: string
  tab: string
  defaultTab: string
}
const emit = defineEmits<Emit>()
const tabObjs = ref<TabObj[]>([
  { name: 'tab_generate_image', defaultTab: t('draw.textToImage'), tab: `${t('draw.textToImage')} ↓` },
])
const tabPanelShow = ref<boolean>(true)

function handleClick(tabOjb: TabObj) {
  tabPanelShow.value = !tabPanelShow.value

  resetTabName(tabOjb)
}

function resetTabName(selectedTabObj: TabObj) {
  tabObjs.value.forEach((element) => {
    if (element.name === selectedTabObj.name)
      selectedTabObj.tab = tabPanelShow.value ? `${selectedTabObj.defaultTab} ↓` : `${selectedTabObj.defaultTab} ↑`
    else
      element.tab = element.defaultTab
  })
}

function handleScrollToBottom() {
  emit('submitted')
}
</script>

<template>
  <div>
    <NTabs type="line" animated default-value="tab_generate_image">
      <NTab v-for="tab in tabObjs" :key="tab.name" :name="tab.name" @click="handleClick(tab)">
        {{ tab.tab }}
      </NTab>
    </NTabs>
    <NTabs type="line" animated default-value="tab_generate_image">
      <NTabPane
        :key="tabObjs[0].name" :name="tabObjs[0].name" display-directive="show"
        :tab-props="{ style: 'display:none' }"
      >
        <transition name="collapse">
          <GenerateImage v-show="tabPanelShow" @submitted="handleScrollToBottom" />
        </transition>
      </NTabPane>
    </NTabs>
  </div>
</template>
