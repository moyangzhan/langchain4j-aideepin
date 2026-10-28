<script setup lang='ts'>
import { NAlert } from 'naive-ui'
import { t } from '@/locales'
</script>

<template>
  <NAlert type="info" :show-icon="false" style="font-size: 12px;">
    <div style="font-weight: 600;">
      {{ t('knowledgeBase.importQaReqTitle') }}
    </div>
    <div>1. {{ t('knowledgeBase.importQaReqHeader') }}</div>
    <div>2. {{ t('knowledgeBase.importQaReqOnePerCell') }}</div>
    <div>3. {{ t('knowledgeBase.importQaReqMqa') }}</div>
    <div style="padding-left: 14px;">
      · {{ t('knowledgeBase.importQaReqBlank') }}
    </div>
    <div style="padding-left: 14px;">
      · {{ t('knowledgeBase.importQaReqMerged') }}
    </div>
    <div style="padding-left: 14px;">
      · {{ t('knowledgeBase.importQaReqSame') }}
    </div>
    <div>{{ t('knowledgeBase.importQaNoSegmentParams') }}</div>
  </NAlert>
</template>
