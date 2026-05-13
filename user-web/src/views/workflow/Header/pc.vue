<script lang="ts" setup>
import { computed, h, ref } from 'vue'
import { NDropdown, NTag, useMessage } from 'naive-ui'
import { HoverButton, SvgIcon } from '@/components/common'
import { emptyWorkflowInfo } from '@/utils/functions'
import { useAuthStore, useUserStore, useWfStore } from '@/store'
import api from '@/api'
import { t } from '@/locales'

interface Props {
  workflow: Workflow.WorkflowInfo
}
interface Emit {
  (ev: 'showView', type: string): void
}
const props = withDefaults(defineProps<Props>(), {
  workflow: () => emptyWorkflowInfo(),
})
const emit = defineEmits<Emit>()
const ms = useMessage()
const wfStore = useWfStore()
const authStore = useAuthStore()
const userStore = useUserStore()
const showViewType = ref<string>('instanceList')
const submitting = ref<boolean>(false)
const options = computed(() => {
  const mine = props.workflow.userUuid === userStore.userInfo.uuid
  const common = [
    {
      label: mine ? t('common.edit') : t('common.view'),
      key: 'edit',
      icon: renderIcon(mine ? 'carbon:edit' : 'carbon:information'),
    },
  ]
  if (authStore.token) {
    common.push({
      label: t('workflow.copyLabel'),
      key: 'copy',
      icon: renderIcon('ri:file-copy-2-line'),
    })
  }
  return common
})

function renderIcon(icon: string) {
  return () => {
    return h(
      SvgIcon,
      {
        icon,
        class: 'text-base cursor-pointer',
      })
  }
}

function toogleView() {
  if (!authStore.checkLoginOrShow())
    return
  showViewType.value = showViewType.value === 'instanceList' ? 'workflowDefine' : 'instanceList'
  emit('showView', showViewType.value)
}

function showEditView() {
  if (!authStore.checkLoginOrShow())
    return
  wfStore.setShowCreateView(true, props.workflow.uuid)
}

async function onCopy() {
  const { data: newWorkflow } = await api.workflowCopy(props.workflow.uuid)
  wfStore.appendWorkflows([newWorkflow], true)
  ms.success(t('workflow.copySuccess'))
}

function handleSelect(key: string | number) {
  if (submitting.value)
    return
  submitting.value = true
  try {
    if (key === 'edit')
      showEditView()
    else if (key === 'copy')
      onCopy()
  } catch (e: any) {
    ms.error(e)
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <header
    class="sticky top-0 left-0 top-0 z-30 border-b dark:border-neutral-800 bg-white/80 dark:bg-black/20 backdrop-blur"
  >
    <div class="relative flex items-center justify-between max-w-screen-xl px-4 m-auto h-10">
      <div class="flex items-center">
        <p class="text-sm">
          {{ workflow?.title ?? '' }}
        </p>
      </div>
      <div class="flex items-center">
        <HoverButton
          placement="left" class="w-[70px] mr-2"
          :tooltip="showViewType === 'instanceList' ? t('workflow.switchToFlow') : t('workflow.switchToRequestList')" @click="toogleView()"
        >
          <div class="text-xl flex items-center space-x-2">
            <NTag v-if="showViewType === 'instanceList'" round :bordered="false" :style="{ cursor: 'pointer' }">
              {{ t('workflow.workflowLabel') }}
              <template #avatar>
                <SvgIcon class="text-xl" icon="carbon:flow" />
              </template>
            </NTag>
            <NTag v-if="showViewType !== 'instanceList'" round :bordered="false" :style="{ cursor: 'pointer' }">
              {{ t('workflow.requestList') }}
              <template #avatar>
                <SvgIcon class="text-xl" icon="si:align-left-detailed-line" />
              </template>
            </NTag>
          </div>
        </HoverButton>
        <NDropdown :options="options" class="mr-2" @select="handleSelect">
          <SvgIcon class="w-6 ml-2 cursor-pointer" icon="ri:more-fill" />
        </NDropdown>
      </div>
    </div>
  </header>
</template>
