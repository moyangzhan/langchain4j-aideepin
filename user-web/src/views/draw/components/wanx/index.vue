<script setup lang='ts'>
import { ref, watch } from 'vue'
import { NTab, NTabPane, NTabs } from 'naive-ui'
import GenerateImage from './GenerateImage.vue'
import GenerateBackground from './GenerateBackground.vue'
import { useAppStore } from '@/store'
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
  { name: 'tab_change_background', defaultTab: t('draw.backgroundGenerate'), tab: t('draw.backgroundGenerate') },
])
const appStore = useAppStore()
const interactingMethod = ref<string>(tabObjs.value[0].name)
const tabPanelShow = ref<boolean>(true)
let lastClickTab = tabObjs.value[0].name

function handleClick(tabOjb: TabObj) {
  console.log(`${interactingMethod.value},${tabOjb.name}`)
  if (lastClickTab === tabOjb.name)
    tabPanelShow.value = !tabPanelShow.value

  resetTabName(tabOjb)
  lastClickTab = tabOjb.name

  // 重置图片模型下拉框选中项
  if (appStore.selectedImageModel.modelName.includes('wanx-background') && tabOjb.name === 'tab_generate_image') {
    const imageModel = appStore.imageModelByPrefix('wanx2')
    if (imageModel)
      appStore.setSelectedImageModel(imageModel.modelId)
  } else if (appStore.selectedImageModel.modelName.includes('wanx2') && tabOjb.name === 'tab_change_background') {
    const imageModel = appStore.imageModelByPrefix('wanx-background')
    if (imageModel)
      appStore.setSelectedImageModel(imageModel.modelId)
  }
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

watch(
  () => appStore.selectedImageModel,
  (newVal) => {
    if (newVal.modelName.includes('wanx-background')) {
      interactingMethod.value = tabObjs.value[1].name
      lastClickTab = tabObjs.value[1].name
      resetTabName(tabObjs.value[1])
    } else if (newVal.modelName.includes('wanx')) {
      interactingMethod.value = tabObjs.value[0].name
      lastClickTab = tabObjs.value[0].name
      resetTabName(tabObjs.value[0])
    }
  },
  { immediate: true },
)
</script>

<template>
  <div>
    <NTabs v-model:value="interactingMethod" type="line" animated default-value="tab_generate_image">
      <NTab v-for="tab in tabObjs" :key="tab.name" :name="tab.name" @click="handleClick(tab)">
        {{ tab.tab }}
      </NTab>
    </NTabs>
    <NTabs v-model:value="interactingMethod" type="line" animated default-value="tab_generate_image">
      <NTabPane
        :key="tabObjs[0].name" :name="tabObjs[0].name" display-directive="show"
        :tab-props="{ style: 'display:none' }"
      >
        <transition name="collapse">
          <GenerateImage v-show="tabPanelShow" @submitted="handleScrollToBottom" />
        </transition>
      </NTabPane>
      <NTabPane
        :key="tabObjs[1].name" :name="tabObjs[1].name" display-directive="show"
        :tab-props="{ style: 'display:none' }"
      >
        <transition name="collapse">
          <GenerateBackground v-show="tabPanelShow" />
        </transition>
      </NTabPane>
    </NTabs>
  </div>
</template>
