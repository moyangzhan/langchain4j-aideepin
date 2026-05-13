<script setup lang="ts">
import { NInput, NInputNumber, NRadio, NRadioGroup } from 'naive-ui'
import NodePropertyInput from '../NodePropertyInput.vue'
import ReferComment from '../ReferComment.vue'
import ReferTooltip from '../ReferTooltip.vue'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigMailSend
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.sender') }}
      </div>
      <div class="text-sm">
        <NRadioGroup v-model:value="nodeConfig.sender_type">
          <NRadio key="sys" :value="1">
            {{ t('workflow.system') }}
          </NRadio>
          <NRadio key="custom" :value="2">
            {{ t('workflow.custom') }}
          </NRadio>
        </NRadioGroup>
      </div>
    </div>
    <div v-show="nodeConfig.sender_type === 2">
      <div class="flex flex-col space-y-2 text-sm border border-gray-200 rounded-md p-2 bg-gray-100 mt-2">
        <div>{{ t('workflow.smtpServer') }}</div>
        <NInput v-model:value="nodeConfig.smtp.host" placeholder="eg: smtp.exmail.qq.com" />
        <div>{{ t('workflow.smtpPort') }}</div>
        <NInputNumber v-model:value="nodeConfig.smtp.port" />
        <div>{{ t('workflow.senderName') }}</div>
        <NInput v-model:value="nodeConfig.sender.name" />
        <div>{{ t('workflow.senderEmail') }}</div>
        <NInput v-model:value="nodeConfig.sender.mail" />
        <div>{{ t('workflow.senderPassword') }}</div>
        <NInput v-model:value="nodeConfig.sender.password" type="password" show-password-on="mousedown" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center items-center">
        {{ t('workflow.recipientEmail') }}<ReferTooltip /><span class="text-red-500 text-base">*</span>
      </div>
      <div>
        <NInput v-model:value="nodeConfig.to_mails" :placeholder="t('workflow.recipientEmailPlaceholder')" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center items-center">
        {{ t('workflow.ccEmail') }}<ReferTooltip />
      </div>
      <div>
        <NInput v-model:value="nodeConfig.cc_mails" :placeholder="t('workflow.recipientEmailPlaceholder')" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1 flex align-center items-center">
        {{ t('workflow.emailSubject') }}
        <ReferTooltip :brief="true" />
        <span class="text-red-500 text-base">*</span>
      </div>
      <div class="flex flex-col">
        <ReferComment />
        <NInput v-model:value="nodeConfig.subject" type="textarea" :autosize="{ minRows: 1, maxRows: 3 }" />
      </div>
    </div>
    <div class="mt-6 mb-12">
      <div class="text-xl mb-1 flex align-center items-center">
        {{ t('workflow.emailContent') }}
        <ReferTooltip :brief="true" />
        <span class="text-red-500 text-base">*</span>
      </div>
      <div class="flex flex-col">
        <ReferComment />
        <NInput v-model:value="nodeConfig.content" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
  </div>
</template>
