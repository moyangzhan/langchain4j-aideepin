<script setup lang="ts">
import { computed, watch } from 'vue'
import { NButton, NIcon, NInput, NInputNumber, NSelect, NTooltip } from 'naive-ui'
import { QuestionCircle16Regular } from '@vicons/fluent'
import NodePropertyInput from '../NodePropertyInput.vue'
import WfImageModelSelector from '../WfImageModelSelector.vue'
import ReferComment from '../ReferComment.vue'
import { TONGYI_WANX_SIZE_OPTIONS } from '@/utils/constant'
import { useAppStore } from '@/store'
import { t } from '@/locales'
interface Props {
  workflow: Workflow.WorkflowInfo
  wfNode: Workflow.WorkflowNode
}
const props = defineProps<Props>()
const nodeConfig = props.wfNode.nodeConfig as Workflow.NodeConfigTongyiwanx
const appStore = useAppStore()

// 尺寸选项随所选模型的 properties.sizes 联动,未配置时回退默认档位
// Size options follow the selected model's properties.sizes; falls back to defaults if unset
const sizeOptions = computed(() => {
  const model = appStore.getImageModelByName(nodeConfig.model_name)
  const modelProperties = model?.properties as { sizes?: { value: string, label: string }[] } | undefined
  return modelProperties?.sizes?.length
    ? modelProperties.sizes.map(item => ({ label: item.label, value: item.value }))
    : TONGYI_WANX_SIZE_OPTIONS
})
// 模型切换后当前尺寸不在可选值时,重置为第一个合法值
// After switching models, reset to the first valid size if the current one is no longer allowed
watch(sizeOptions, (opts) => {
  if (opts.length && !opts.some(item => item.value === nodeConfig.size))
    nodeConfig.size = opts[0].value
}, { immediate: true })

function selected(modelName: string) {
  console.log('nodeConfig.modelName', nodeConfig.model_name, modelName)
  nodeConfig.model_name = modelName
}
</script>

<template>
  <div class="flex flex-col w-full">
    <NodePropertyInput :workflow="workflow" :wf-node="wfNode" />
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.model') }}
      </div>
      <div>
        <WfImageModelSelector
          :model-name="nodeConfig.model_name" platform="dashscope"
          @image-model-selected="selected"
        />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.promptLabel') }}
        <NTooltip trigger="hover">
          <template #trigger>
            <NIcon style="padding-top: 0.1rem">
              <QuestionCircle16Regular />
            </NIcon>
          </template>
          <div>{{ t('workflow.promptEmptyUsePrevious') }}</div>
        </NTooltip>
      </div>
      <div class="flex flex-col">
        <ReferComment />
        <NInput v-model:value="nodeConfig.prompt" type="textarea" :autosize="{ minRows: 3, maxRows: 10 }" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.imageSize') }}
      </div>
      <div>
        <NSelect v-model:value="nodeConfig.size" :options="sizeOptions" />
      </div>
    </div>
    <div class="mt-6">
      <div class="text-xl mb-1">
        {{ t('workflow.randomSeed') }}
      </div>
      <div class="flex space-x-2 items-center">
        <NInputNumber v-model:value="nodeConfig.seed" :min="-1" :max="2147483647" class="grow" />
        <NButton type="primary" size="tiny" ghost @click="nodeConfig.seed = Math.floor(Math.random() * 2147483647)">
          {{ t('workflow.randomGenerate') }}
        </NButton>
      </div>
    </div>
  </div>
</template>
