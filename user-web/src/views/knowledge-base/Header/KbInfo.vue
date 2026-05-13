<script setup lang='ts'>
import { ref, watch } from 'vue'
import { NAvatar, NButton, NDivider, NFlex, NIcon, NModal, NPopconfirm, NTag, NTooltip } from 'naive-ui'
import { Star24Filled, Star24Regular } from '@vicons/fluent'
import { Bookmarks, VectorBeizer2 } from '@vicons/tabler'
import { useKbStore } from '@/store'
import { knowledgeBaseEmptyInfo } from '@/utils/functions'
import defaultAvatar from '@/assets/avatar.jpg'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  showModal: boolean
  knowledgeBase: KnowledgeBase.Info
}
interface Emit {
  (ev: 'showModal', show: boolean): void
}
const props = withDefaults(defineProps<Props>(), {
  showModal: false,
  knowledgeBase: () => knowledgeBaseEmptyInfo(),
})
const emit = defineEmits<Emit>()
const kbStore = useKbStore()
const innerShow = ref<boolean>(props.showModal)

async function clearHistory(kbInfo: KnowledgeBase.Info) {
  await api.knowledgeBaseQaRecordClear<boolean>()
  kbStore.clearRecords(kbInfo.uuid)
}
async function handleClickStar(kbInfo: KnowledgeBase.Info) {
  const starOrUnstarResp = await api.knowledgeBaseStar<boolean>(kbInfo.uuid)
  const starOrUnstar = starOrUnstarResp.data
  kbStore.insertOrUpdateStarInfo({ kbUuid: kbInfo.uuid, kbTitle: kbInfo.title, star: starOrUnstar })
  kbInfo.starCount = starOrUnstar ? kbInfo.starCount + 1 : kbInfo.starCount - 1
}

watch(() => props.showModal, (val) => {
  innerShow.value = val
})
watch(() => innerShow.value, (val) => {
  if (!val)
    emit('showModal', false)
})
</script>

<template>
  <NModal v-model:show="innerShow" :title="knowledgeBase.title" style="width: 90%; max-width: 640px" preset="card">
    <NFlex vertical>
      <NFlex justify="space-between">
        <NTag size="large" :bordered="false" :color="{ color: '#ff000000' }">
          {{ knowledgeBase.ownerName }}
          <template #avatar>
            <NAvatar
              :src="`/api/user/avatar/${knowledgeBase.ownerUuid}`" size="large" :fallback-src="defaultAvatar"
              color="#ff0000000"
            />
          </template>
        </NTag>
        <NFlex>
          <NTooltip trigger="hover">
            <template #trigger>
              <NTag size="medium" :bordered="false" round :color="{ color: '#ff000000' }">
                {{ knowledgeBase.itemCount }}
                <template #icon>
                  <NIcon :component="Bookmarks" depth="2" />
                </template>
              </NTag>
            </template>
            {{ t('knowledgeBase.knowledgePoint') }}
          </NTooltip>
          <NTooltip trigger="hover">
            <template #trigger>
              <NTag size="medium" :bordered="false" round :color="{ color: '#ff000000' }">
                {{ knowledgeBase.embeddingCount }}
                <template #icon>
                  <NIcon :component="VectorBeizer2" depth="2" />
                </template>
              </NTag>
            </template>
            {{ t('knowledgeBase.vector') }}
          </NTooltip>
          <NTag
            size="medium" :bordered="false" round :color="{ color: '#ff000000' }" checkable
            @click="handleClickStar(knowledgeBase)"
          >
            {{ knowledgeBase.starCount }}
            <template #icon>
              <NIcon v-show="!kbStore.kbUuidToStarInfo.get(knowledgeBase.uuid)?.star" :component="Star24Regular" />
              <NIcon
                v-show="kbStore.kbUuidToStarInfo.get(knowledgeBase.uuid)?.star" :component="Star24Filled"
                color="#eac54f"
              />
            </template>
          </NTag>
        </NFlex>
      </NFlex>
      <NFlex>
        <NTooltip trigger="hover">
          <template #trigger>
            <NTag size="small" :bordered="false">
              {{ knowledgeBase.isPublic ? t('common.public') : t('common.private') }}
            </NTag>
          </template>
          {{ t('knowledgeBase.publicDesc') }}<br>
          {{ t('knowledgeBase.privateDesc') }}
        </NTooltip>
        <NTooltip trigger="hover">
          <template #trigger>
            <NTag size="small" :bordered="false">
              {{ knowledgeBase.isStrict ? t('knowledgeBase.strictMode') : t('knowledgeBase.looseMode') }}
            </NTag>
          </template>
          {{ t('knowledgeBase.strictModeDescShort') }}<br>
          {{ t('knowledgeBase.looseModeDescShort') }}
        </NTooltip>
        <NTooltip trigger="hover">
          <template #trigger>
            <NTag size="small" :bordered="false">
              {{ t('knowledgeBase.maxRecallCount') }}{{ knowledgeBase.retrieveMaxResults === 0 ? '-' : knowledgeBase.retrieveMaxResults }}
            </NTag>
          </template>
          {{ t('knowledgeBase.maxRecallCountTip') }}<br>
        </NTooltip>
        <NTooltip trigger="hover">
          <template #trigger>
            <NTag size="small" :bordered="false">
              {{ t('knowledgeBase.minRecallScore') }}{{ knowledgeBase.retrieveMinScore === 0 ? '-' : knowledgeBase.retrieveMinScore }}
            </NTag>
          </template>
          {{ t('knowledgeBase.minRecallScoreTip') }}
        </NTooltip>
      </NFlex>
      <NDivider />
      <div>{{ knowledgeBase.remark }}</div>
    </NFlex>
    <template #footer>
      <NPopconfirm placement="top" @positive-click="clearHistory(knowledgeBase)">
        <template #trigger>
          <NButton size="small" text type="primary">
            {{ t('knowledgeBase.clearHistory') }}
          </NButton>
        </template>
        {{ t('knowledgeBase.deleteNotRecoverCaution') }}
      </NPopconfirm>
    </template>
  </NModal>
</template>
