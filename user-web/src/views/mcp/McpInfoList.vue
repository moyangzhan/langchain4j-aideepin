<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { NButton, NPagination, useLoadingBar, useMessage } from 'naive-ui'
import { useMcpStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'
import { debounce } from '@/utils/functions/debounce'

const emit = defineEmits<Emit>()
const ms = useMessage()
const loaddingBar = useLoadingBar()
const mcpStore = useMcpStore()
const mcpInfoList = ref<Mcp.McpInfo[]>([])
const currentPage = ref<number>(1)
const totalPage = ref<number>(0)
const pageSize = 21

interface Emit {
  (ev: 'showInfoModal', mcpInfo: Mcp.McpInfo): void
  (ev: 'showConfigModal', mcpInfo: Mcp.McpInfo): void
}
/**
 * 加载公开列表
 */
async function loadMcpPage(page: number) {
  if (mcpStore.loading)
    return
  console.log('loadMcpPage', page)
  loaddingBar.start()
  mcpStore.setLoading(true)
  try {
    if (page > 1000) {
      ms.warning(t('mcp.maxPageLimit'), {
        duration: 3000,
      })
      return
    }
    const { data } = await api.mcpSearch<Mcp.McpInfoListResp>('', page, pageSize)
    data.records.forEach((mcp) => {
      const userMcp = mcpStore.myUserMcpList.find(userMcp => userMcp.mcpId === mcp.id)
      if (userMcp) {
        mcp.configured = true
        mcp.customizedParamDefinitions.forEach((uninitParam) => {
          const paramSetting = userMcp.mcpCustomizedParams.find(varItem => varItem.name === uninitParam.name)
          // 将已设置好的参数赋值给mcp的customizedParamDefinition
          if (paramSetting)
            uninitParam.value = paramSetting.value
          else
            uninitParam.value = ''
        })
      } else {
        mcp.configured = false
      }
    })
    mcpInfoList.value = data.records
    totalPage.value = data.pages
  } catch (error) {
    console.error(error)
  } finally {
    mcpStore.setLoading(false)
    loaddingBar.finish()
  }
}

function onShowInfoModal(mcpInfo: Mcp.McpInfo) {
  emit('showInfoModal', mcpInfo)
}

function onShowConfigModal(mcpInfo: Mcp.McpInfo) {
  emit('showConfigModal', mcpInfo)
}

const handleLoadNext = debounce(loadMcpPage, 300)
onMounted(() => {
  if (mcpInfoList.value.length === 0)
    handleLoadNext(currentPage.value)
})
</script>

<template>
  <div class="flex flex-col w-full h-full pb-3">
    <div class="flex flex-wrap justify-start items-start overflow-y-auto">
      <div
        v-for="mcpInfo in mcpInfoList" :key="mcpInfo.uuid"
        class="m-2 flex flex-col space-y-2 border border-gray-200 p-3 rounded-md h-[180px] w-[380px] hover:bg-orange-50"
      >
        <div class="font-bold text-base">
          {{ mcpInfo.title }}
        </div>
        <div class="h-[100px] overflow-hidden text-sm">
          {{ mcpInfo.remark }}
        </div>
        <div class="flex justify-end space-x-2">
          <NButton size="tiny" quaternary type="primary" @click="onShowInfoModal(mcpInfo)">
            {{ t('common.detail') }}
          </NButton>
          <NButton size="tiny" quaternary type="primary" @click="onShowConfigModal(mcpInfo)">
            <span v-if="mcpInfo.configured">{{ t('mcp.configLabel') }}</span>
            <span v-if="!mcpInfo.configured">{{ t('mcp.statusEnable') }}</span>
          </NButton>
        </div>
      </div>
    </div>
    <div class="flex justify-end w-full pr-2">
      <NPagination
        v-show="totalPage > 1" v-model:page="currentPage" :page-size="pageSize" :page-count="totalPage"
        @update:page="loadMcpPage"
      />
    </div>
  </div>
</template>
