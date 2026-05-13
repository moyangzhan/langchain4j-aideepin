<script setup lang='ts'>
import { computed, ref } from 'vue'
import { useRoute } from 'vue-router'
import HeaderComponent from './Header/index.vue'
import PCHeader from './Header/pc.vue'
import WfRuntimeList from './WfRuntimeList.vue'
import WorkflowDefine from './WorkflowDefine.vue'
import LoginTip from '@/views/user/LoginTip.vue'
import { useBasicLayout } from '@/hooks/useBasicLayout'
import { useAuthStore, useWfStore } from '@/store'
import { emptyWorkflowInfo } from '@/utils/functions'

console.log('workflow index')
const route = useRoute()
const workflowStore = useWfStore()
const authStore = useAuthStore()
const { isMobile } = useBasicLayout()
const { uuid: currWfUuid, viewType } = route.params as { uuid: string; viewType: string }
const selectedViewType = ref(!viewType ? 'instanceList' : viewType)
console.log('authStore.token', authStore.token)
const currWorkflowInfo = computed(() => {
  return workflowStore.getWorkflowInfo(currWfUuid) || emptyWorkflowInfo()
})
</script>

<template>
  <div class="chat-box flex flex-col w-full h-full">
    <HeaderComponent v-if="isMobile" :using-context="false" />
    <PCHeader v-else :workflow="currWorkflowInfo" @show-view="(viewType) => selectedViewType = viewType" />
    <template v-if="!authStore.token">
      <LoginTip />
    </template>
    <template v-if="authStore.token && currWorkflowInfo.uuid">
      <WfRuntimeList
        :workflow="currWorkflowInfo"
        :show="selectedViewType === 'instanceList'"
      />
      <WorkflowDefine v-show="selectedViewType === 'workflowDefine'" :workflow="currWorkflowInfo" />
    </template>
  </div>
</template>
