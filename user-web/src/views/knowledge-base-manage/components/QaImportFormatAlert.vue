<script setup lang='ts'>
import { NCollapse, NCollapseItem } from 'naive-ui'
import { t } from '@/locales'
</script>

<template>
  <NCollapse>
    <NCollapseItem name="formatReq">
      <template #header>
        <span style="font-size: 12px; font-weight: 600;">{{ t('knowledgeBase.importQaReqTitle') }}</span>
      </template>
      <div class="flex flex-col" style="font-size: 12px; gap: 2px;">
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
      </div>
    </NCollapseItem>
  </NCollapse>
</template>
