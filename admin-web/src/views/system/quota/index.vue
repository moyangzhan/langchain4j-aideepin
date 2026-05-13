<template>
  <div>
    <n-alert :title="t('common.tip')" type="info" :bordered="true" closable>
      {{ t('system.configEffectTip') }}
    </n-alert>
    <n-grid :x-gap="24">
      <n-grid-item span="6">
        <n-card :bordered="false" size="small" class="proCard">
          <n-thing
            class="thing-cell"
            v-for="item in typeTabList"
            :key="item.key"
            :class="{ 'thing-cell-on': type === item.key }"
            @click="switchType(item)"
          >
            <template #header>{{ item.name }}</template>
            <template #description>{{ item.desc }}</template>
          </n-thing>
        </n-card>
      </n-grid-item>
      <n-grid-item span="18">
        <n-card :bordered="false" size="small" :title="typeTitle" class="proCard">
          <TokenSetting v-if="type === 1" :quota="tokenQuota" @reload-config="reloadConfig" />
          <RequestSetting v-if="type === 2" :quota="requestQuota" @reload-config="reloadConfig" />
          <ImageGenerateSetting
            v-if="type === 3"
            :quota="imageGenerateQuota"
            @reload-config="reloadConfig"
          />
          <KnowledgeBaseSetting v-if="type === 4" :quota="qaQuota" @reload-config="reloadConfig" />
        </n-card>
      </n-grid-item>
    </n-grid>
  </div>
</template>
<script lang="ts" setup>
  import { ref, onMounted, reactive, computed } from 'vue'
  import TokenSetting from './TokenSetting.vue'
  import ImageGenerateSetting from './ImageGenerateSetting.vue'
  import RequestSetting from './RequestSetting.vue'
  import KnowledgeBaseSetting from './KnowledgeBaseSetting.vue'
  import api from '@/api/sysConfig.js'
  import { QuotaConfig } from '/#/sysConfig'
  import { t } from '@/locales'

  const typeTabList = computed(() => [
    {
      name: t('system.tokenQuota'),
      desc: t('system.tokenQuotaDesc'),
      key: 1,
    },
    {
      name: t('system.requestQuota'),
      desc: t('system.requestQuotaDesc'),
      key: 2,
    },
    {
      name: t('system.imageQuota'),
      desc: t('system.imageQuotaDesc'),
      key: 3,
    },
    {
      name: t('system.chatQuota'),
      desc: t('system.chatQuotaDesc'),
      key: 4,
    },
  ])

  const type = ref(1)
  const typeTitle = computed(
    () => typeTabList.value.find((item) => item.key === type.value)?.name || ''
  )
  const tokenQuota = ref<QuotaConfig>({
    daily: 0,
    monthly: 0,
  })
  const imageGenerateQuota = reactive({
    daily: 0,
    monthly: 0,
  })
  const requestQuota = reactive({
    daily: 0,
    monthly: 0,
  })
  const qaQuota = reactive({
    daily: 0,
    monthly: 0,
  })
  function switchType(e) {
    type.value = e.key
  }
  async function reloadConfig() {
    const { records } = await api.search({ keyword: 'quota_by' }, 1, 100)
    console.log(records)
    records.forEach((element) => {
      const val = parseInt(element.value)
      if (element.name === 'quota_by_token_daily') {
        tokenQuota.value.daily = val
      } else if (element.name === 'quota_by_token_monthly') {
        tokenQuota.value.monthly = val
      } else if (element.name === 'quota_by_request_daily') {
        requestQuota.daily = val
      } else if (element.name === 'quota_by_request_monthly') {
        requestQuota.monthly = val
      } else if (element.name === 'quota_by_image_daily') {
        imageGenerateQuota.daily = val
      } else if (element.name === 'quota_by_image_monthly') {
        imageGenerateQuota.monthly = val
      } else if (element.name === 'quota_by_qa_ask_daily') {
        qaQuota.daily = val
      } else if (element.name === 'quota_by_qa_item_monthly') {
        qaQuota.monthly = val
      }
    })
  }
  onMounted(async () => {
    reloadConfig()
  })
</script>
<style lang="less" scoped>
  .thing-cell {
    margin: 0 -16px 10px;
    padding: 5px 16px;

    &:hover {
      background: #f3f3f3;
      cursor: pointer;
    }
  }

  .thing-cell-on {
    background: #f0faff;
    color: #2d8cf0;

    ::v-deep(.n-thing-main .n-thing-header .n-thing-header__title) {
      color: #2d8cf0;
    }

    &:hover {
      background: #f0faff;
    }
  }
</style>
