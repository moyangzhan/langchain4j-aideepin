<script setup lang='ts'>
import { nextTick, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { storeToRefs } from 'pinia'
import { NTabPane, NTabs } from 'naive-ui'
import SubList from './SubList.vue'
import { useAuthStore, useKbStore } from '@/store'
import { t } from '@/locales'
import api from '@/api'

const route = useRoute()
const router = useRouter()
const currentPage = ref<number>(1)
const pageSize = 20
const kbStore = useKbStore()
const { activeKbUuid, myKbInfos, publicKbInfos, selectedKbType } = storeToRefs<any>(kbStore)
const authStore = useAuthStore()
const authStoreRef = ref<AuthState>(authStore)
const { kbUuid: currKbUuid } = route.params as { kbUuid: string }

// F5 reload
if (currKbUuid !== 'default' && kbStore.activeKbUuid === 'default')
  kbStore.setActive(currKbUuid)

async function initList() {
  if (kbStore.loaddingKbList || kbStore.myKbInfos.length > 0)
    return

  kbStore.setLoadingKbList(true)
  try {
    const { data } = await api.knowledgeBaseSearchMine<KnowledgeBase.InfoListResp>('', currentPage.value, pageSize)
    if (data.records) {
      kbStore.setMyKbInfos(data.records)
      nextTick(() => {
        if (activeKbUuid.value === 'default') {
          kbStore.setActive(myKbInfos.value[0].uuid)
          router.replace({ name: 'QADetail', params: { kbUuid: activeKbUuid.value } })
        }
      })
    }
  } finally {
    kbStore.setLoadingKbList(false)
  }
}

async function initStarredList() {
  const starListResp = await api.knowledgeBaseStarListMine<KnowledgeBase.KbStarListResp>(1, 100)
  kbStore.appStarInfos(starListResp.data.records)
}

async function initPublicList() {
  const { data: publicData } = await api.knowledgeBaseSearchPublic<KnowledgeBase.InfoListResp>('', currentPage.value, pageSize)
  if (publicData.records)
    kbStore.setPublicKbInfos(publicData.records)
}

watch(
  () => authStoreRef.value.token,
  (newVal) => {
    if (newVal) {
      initList()
      initStarredList()
    }
  },
)

watch(
  () => kbStore.reloadKbInfosSignal,
  (newVal) => {
    if (newVal) {
      try {
        initList()
        initStarredList()
      } finally {
        kbStore.setReloadKbInfosSignal(false)
      }
    }
  },
)

onMounted(() => {
  console.log('list onMounted')
  initPublicList()
  if (authStoreRef.value.token) {
    initList()
    initStarredList()
  }
})
</script>

<template>
  <NTabs v-model:value="selectedKbType" tab-class="h-10" pane-class="h-full" type="line" justify-content="space-evenly" class="kb-sider-tabs">
    <NTabPane name="mine" :tab="t('common.mine')" size="small">
      <SubList :list="myKbInfos" :active-kb-uuid="activeKbUuid" />
    </NTabPane>
    <NTabPane name="public" :tab="t('common.public')">
      <SubList :list="publicKbInfos" :active-kb-uuid="activeKbUuid" />
    </NTabPane>
  </NTabs>
</template>

<style scoped>
.kb-sider-tabs {
  display: flex;
  flex-direction: column;
  height: 100%;
}
</style>

<style>
.kb-sider-tabs .n-tabs-pane-wrapper {
  flex: 1 !important;
  min-height: 0 !important;
  overflow: hidden !important;
}
.kb-sider-tabs .n-tab-pane {
  height: 100% !important;
  overflow: hidden !important;
}
</style>
