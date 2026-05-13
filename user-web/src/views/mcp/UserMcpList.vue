<script setup lang='ts'>
import { NButton, NIcon } from 'naive-ui'
import { Cat } from '@vicons/fa'
import { useAuthStore, useMcpStore } from '@/store'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import LoginTip from '@/views/user/LoginTip.vue'
import { t } from '@/locales'

const emit = defineEmits<Emit>()
const authStore = useAuthStore()
const mcpStore = useMcpStore()
const { isMobile } = useBasicLayout()
interface Emit {
  (ev: 'showInfoModal', mcpInfo: Mcp.McpInfo): void
  (ev: 'showConfigModal', mcpInfo: Mcp.McpInfo): void
}

function onShowInfoModal(mcpInfo: Mcp.McpInfo) {
  emit('showInfoModal', mcpInfo)
}

function onShowConfigModal(mcpInfo: Mcp.McpInfo) {
  emit('showConfigModal', mcpInfo)
}
</script>

<template>
  <div class="flex flex-col w-full h-full pb-3">
    <div class="flex flex-wrap justify-start items-start overflow-y-auto">
      <div v-if="!authStore.token" class="w-full max-w-screen-xl m-auto" :class="[isMobile ? 'p-2' : 'p-4']">
        <LoginTip />
      </div>
      <div
        v-if="authStore.token && mcpStore.myUserMcpList.length === 0"
        class="flex items-center justify-center mt-4 text-center text-neutral-400" :class="[isMobile ? 'p-2' : 'p-4']"
      >
        <NIcon :component="Cat" size="32" />{{ t('common.noData') }}
      </div>
      <div
        v-for="userMcp in mcpStore.myUserMcpList" :key="userMcp.uuid"
        class="m-2 flex flex-col space-y-2 border border-gray-200 p-3 rounded-md h-[180px] w-[380px] hover:bg-orange-50"
      >
        <div class="font-bold text-base">
          {{ userMcp.mcpInfo.title }}
        </div>
        <div class="h-[100px] overflow-hidden text-sm">
          {{ userMcp.mcpInfo.remark }}
        </div>
        <div class="flex justify-end space-x-2">
          <NButton size="tiny" quaternary type="primary" @click="onShowInfoModal(userMcp.mcpInfo)">
            {{ t('common.detail') }}
          </NButton>
          <NButton size="tiny" quaternary type="primary" @click="onShowConfigModal(userMcp.mcpInfo)">
            {{ t('mcp.configLabel') }}
          </NButton>
        </div>
      </div>
    </div>
  </div>
</template>
