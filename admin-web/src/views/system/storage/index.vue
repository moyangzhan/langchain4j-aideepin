<template>
  <div>
    <n-alert :title="t('common.tip')" type="info" :bordered="true">
      {{ t('system.currentStorageLocation')
      }}<span class="font-bold">{{
        selectedStorageLocation === 1 ? t('system.localStorage') : t('system.aliyunOss')
      }}</span>
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
          <LocalConfig v-if="type === 1" @reload="loadData" />
          <AliyunOssConfig v-if="type === 2" @reload="loadData" />
        </n-card>
      </n-grid-item>
    </n-grid>
  </div>
</template>
<script lang="ts" setup>
  import { onMounted, ref, computed } from 'vue'
  import LocalConfig from './LocalConfig.vue'
  import AliyunOssConfig from './AliyunOssConfig.vue'
  import api from '@/api/sysConfig.js'
  import { t } from '@/locales'

  const typeTabList = computed(() => [
    {
      name: t('system.localStorage'),
      desc: t('system.localStorageDesc'),
      key: 1,
    },
    {
      name: t('system.aliyunOss'),
      desc: t('system.aliyunOssDesc'),
      key: 2,
    },
  ])

  const selectedStorageLocation = ref<number>(1)
  const type = ref<number>(1)
  const typeTitle = computed(
    () => typeTabList.value.find((item) => item.key === type.value)?.name || ''
  )
  function switchType(e) {
    type.value = e.key
  }
  async function loadData() {
    const { data: records } = await api.search({ names: ['storage_location'] }, 1, 10)
    if (records.length === 0) {
      console.log('can not find storage_location confing')
      return
    }
    selectedStorageLocation.value = parseInt(records[0].value)
  }
  onMounted(() => {
    loadData()
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
